package uk.gov.ons.addressIndex.server.controllers

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.ons.addressIndex.model.db.index.HybridAddressCollection
import uk.gov.ons.addressIndex.model.server.response.address._
import uk.gov.ons.addressIndex.server.model.dao.QueryValues
import uk.gov.ons.addressIndex.server.modules.response.AddressControllerResponse
import uk.gov.ons.addressIndex.server.modules.validation.AddressControllerValidation
import uk.gov.ons.addressIndex.server.modules._
import uk.gov.ons.addressIndex.server.utils.{APIThrottle, AddressAPILogger, ConfidenceScoreHelper, HopperScoreHelper}

import scala.concurrent.{ExecutionContext, Future}
import scala.math._
import scala.util.Try
import scala.util.control.NonFatal

@Singleton
class AddressController @Inject()(val controllerComponents: ControllerComponents,
                                  esRepo: ElasticsearchRepository,
                                  parser: ParserModule,
                                  conf: ConfigModule,
                                  versionProvider: VersionModule,
                                  overloadProtection: APIThrottle,
                                  addressValidation: AddressControllerValidation
                                 )(implicit ec: ExecutionContext)
  extends PlayHelperController(versionProvider) with AddressControllerResponse {

  lazy val logger: AddressAPILogger = AddressAPILogger("address-index-server:AddressController")

  val missing: String = "missing"
  val invalid: String = "invalid"
  val valid: String = "valid"
  val notRequired: String = "not required"

  /**
    * Address query API
    *
    * @param input the address query
    * @return Json response with addresses information
    */
  def addressQuery(implicit input: String,
                   offset: Option[String] = None,
                   limit: Option[String] = None,
                   classificationfilter: Option[String] = None,
                   rangekm: Option[String] = None,
                   lat: Option[String] = None,
                   lon: Option[String] = None,
                   historical: Option[String] = None,
                   matchthreshold: Option[String] = None,
                   verbose: Option[String] = None,
                   epoch: Option[String] = None,
                   includeauxiliarysearch: Option[String] = None,
                   eboost: Option[String] = None,
                   nboost: Option[String] = None,
                   sboost: Option[String] = None,
                   wboost: Option[String] = None
                  ): Action[AnyContent] = Action async { implicit req =>

    val clusterId = conf.config.elasticSearch.clusterPolicies.address

    val startingTime: Long = System.currentTimeMillis()
    val ip: String = req.remoteAddress
    val url: String = req.uri

    // get the defaults and maxima for the paging parameters from the config
    val defLimit = conf.config.elasticSearch.defaultLimit
    val defOffset = conf.config.elasticSearch.defaultOffset
    val defThreshold = conf.config.elasticSearch.matchThreshold

    val limVal = limit.getOrElse(defLimit.toString)
    val offVal = offset.getOrElse(defOffset.toString)
    val threshVal = matchthreshold.getOrElse(defThreshold.toString)

    val filterString = classificationfilter.getOrElse("").replaceAll("\\s+", "")
    val endpointType = "address"

    val hist = historical.flatMap(x => Try(x.toBoolean).toOption).getOrElse(true)
    val verb = verbose.flatMap(x => Try(x.toBoolean).toOption).getOrElse(false)
    val auxiliary = includeauxiliarysearch.flatMap(x => Try(x.toBoolean).toOption).getOrElse(false)

    val sigmoidScaleFactor = conf.config.elasticSearch.scaleFactor

    // validate radius parameters
    val rangeVal = rangekm.getOrElse("")
    val latVal = lat.getOrElse("")
    val lonVal = lon.getOrElse("")

    val epochVal = epoch.getOrElse("")

    val eboostVal = {if (eboost.getOrElse("1.0").isEmpty) "1.0" else eboost.getOrElse("1.0")}
    val nboostVal = {if (nboost.getOrElse("1.0").isEmpty) "1.0" else nboost.getOrElse("1.0")}
    val sboostVal = {if (sboost.getOrElse("1.0").isEmpty) "1.0" else sboost.getOrElse("1.0")}
    val wboostVal = {if (wboost.getOrElse("1.0").isEmpty) "1.0" else wboost.getOrElse("1.0")}

    val eboostDouble = Try(eboostVal.toDouble).toOption.getOrElse(1.0D)
    val nboostDouble = Try(nboostVal.toDouble).toOption.getOrElse(1.0D)
    val sboostDouble = Try(sboostVal.toDouble).toOption.getOrElse(1.0D)
    val wboostDouble = Try(wboostVal.toDouble).toOption.getOrElse(1.0D)

    def writeLog(badRequestErrorMessage: String = "", formattedOutput: String = "", numOfResults: String = "", score: String = "", activity: String = ""): Unit = {
      val authVal = req.headers.get("authorization").getOrElse("Anon")
      val authHasPlus = authVal.indexOf("+") > 0
      val networkId = Try(if (authHasPlus) authVal.split("\\+")(0) else authVal.split("_")(0)).getOrElse("")
      val organisation = Try(if (authHasPlus) networkId.split("_")(1) else "not set").getOrElse("")

      logger.systemLog(ip = ip, url = url, responseTimeMillis = (System.currentTimeMillis() - startingTime).toString,
        input = input, offset = offVal, limit = limVal, filter = filterString,
        historical = hist, epoch = epochVal, rangekm = rangeVal, lat = latVal, lon = lonVal,
        badRequestMessage = badRequestErrorMessage, formattedOutput = formattedOutput,
        numOfResults = numOfResults, score = score, networkid = networkId, organisation = organisation,
        verbose = verb, eboost = eboostVal, nboost = nboostVal, sboost = sboostVal, wboost = wboostVal,
        endpoint = endpointType, activity = activity, clusterid = clusterId, includeAuxiliary = auxiliary)
    }

    def trimAddresses(fullAddresses: Seq[AddressResponseAddress]): Seq[AddressResponseAddress] = {
      fullAddresses.map { address => address.copy(nag = None, paf = None, nisra = None, relatives = None, crossRefs = None, auxiliary = None) }
    }

    def adjustWithAuxiliaryResults(addresses: Seq[AddressResponseAddress]) =  {
      val extractedAuxiliaryAddresses = addresses.filter(_.auxiliary.isDefined)
      if (extractedAuxiliaryAddresses.isEmpty) addresses
      else {
        val extractedMainAddresses = addresses.filter(_.auxiliary.isEmpty)
        if (extractedMainAddresses.isEmpty) extractedAuxiliaryAddresses
        else {
          val mainIndexMaxUnderlyingScore = extractedMainAddresses.maxBy(_.underlyingScore).underlyingScore
          val auxIndexMinUnderlyingScore = extractedAuxiliaryAddresses.minBy(_.underlyingScore).underlyingScore.toInt
          val adjustedAuxAddresses = extractedAuxiliaryAddresses.map { auxAddress =>
            val increase = ((mainIndexMaxUnderlyingScore + auxAddress.underlyingScore) - auxIndexMinUnderlyingScore) / 1000
            auxAddress.copy(underlyingScore = mainIndexMaxUnderlyingScore + increase)
          }
          extractedMainAddresses ++ adjustedAuxAddresses
        }
      }
    }

    val limitInt = Try(limVal.toInt).toOption.getOrElse(defLimit)
    val offsetInt = Try(offVal.toInt).toOption.getOrElse(defOffset)
    val thresholdFloat = Try(threshVal.toFloat).toOption.getOrElse(defThreshold)

    val queryValues = QueryValues(
      epoch = Some(epochVal),
      filter = Some(filterString),
      historical = Some(hist),
      limit = Some(limitInt),
      offset = Some(offsetInt),
      verbose = Some(verb),
      rangeKM = Some(rangeVal),
      latitude = Some(latVal),
      longitude = Some(lonVal),
      matchThreshold = Some(thresholdFloat),
      includeAuxiliarySearch = Some(auxiliary),
      eboost = Some(eboostDouble),
      nboost = Some(nboostDouble),
      sboost = Some(sboostDouble),
      wboost = Some(wboostDouble)
    )

    val args = AddressArgs(
      input = input,
      tokens = Map.empty, // temporary, filled later
      region = Region.fromStrings(rangeVal, latVal, lonVal),
      epoch = epochVal,
      historical = hist,
      verbose = verb,
      filters = filterString,
      start = offsetInt, // temporary, but zeroed later?
      limit = limitInt, // temporary, expanded later
      queryParamsConfig = None,
      includeAuxiliarySearch = auxiliary,
      eboost = eboostDouble,
      nboost = nboostDouble,
      sboost = sboostDouble,
      wboost = wboostDouble,
      auth = req.headers.get("authorization").getOrElse("Anon")
    )

    val result: Option[Future[Result]] =
      addressValidation.validateAddressFilter(classificationfilter, queryValues)
        .orElse(addressValidation.validateThreshold(matchthreshold, queryValues))
        .orElse(addressValidation.validateRange(rangekm, queryValues))
        .orElse(addressValidation.validateSource(queryValues))
        .orElse(addressValidation.validateKeyStatus(queryValues))
        .orElse(addressValidation.validateLimit(limit, queryValues))
        .orElse(addressValidation.validateOffset(offset, queryValues))
        .orElse(addressValidation.validateInput(input, queryValues))
        .orElse(addressValidation.validateLocation(lat, lon, rangekm, queryValues))
        .orElse(addressValidation.validateEpoch(queryValues))
        .orElse(addressValidation.validateBoosts(eboost,nboost,sboost,wboost,queryValues))
        .orElse(None)

    result match {
      case Some(res) =>
        res // a validation error

      case _ =>
        val tokens = if (input.isEmpty && rangeVal != "" && latVal != "" && lonVal != "" && filterString != "") {
          parser.parse("*")
        } else {
          parser.parse(input)
        }

        // try to get enough results to accurately calculate the hybrid score (may need to be more sophisticated)
        val minimumSample = conf.config.elasticSearch.minimumSample
        val limitExpanded = if (auxiliary) 100 else max(offsetInt + (limitInt * 2), minimumSample)

        val finalArgs = args.copy(
          tokens = tokens,
          start = 0,
          limit = limitExpanded,
          isBlank = input.isEmpty && rangeVal != "" && latVal != "" && lonVal != "" && filterString != ""
        )

        val request: Future[HybridAddressCollection] =
          overloadProtection.breaker.withCircuitBreaker(esRepo.runMultiResultQuery(finalArgs))

        request.map {
          case HybridAddressCollection(hybridAddresses, aggregations@_, maxScore, total@_) =>
            val addresses: Seq[AddressResponseAddress] = hybridAddresses.map(
              AddressResponseAddress.fromHybridAddress(_, verbose = true)
            )

            // ensure that auxiliary index results are scored appropriately
            val auxAdjustedAddresses =  adjustWithAuxiliaryResults(addresses)

            //  calculate the elastic denominator value which will be used when scoring each address
            val elasticDenominator =
              Try(ConfidenceScoreHelper.calculateElasticDenominator(auxAdjustedAddresses.map(_.underlyingScore))).getOrElse(1D)

            // calculate the Hopper and hybrid scores for each  address
            val scoredAddresses = HopperScoreHelper.getScoresForAddresses(auxAdjustedAddresses, tokens, elasticDenominator,sigmoidScaleFactor)

            // work out the threshold for accepting matches (default 5% -> 0.05)
            val threshold = Try(thresholdFloat.toDouble).getOrElse(5.0D)

            // filter out scores below threshold, sort the resultant collection, highest score first
            val sortedAddresses =
               // for range / classification only filter sort by nearest first (underlying score contains distance) and set confidence score to 1
              if (finalArgs.isBlank) auxAdjustedAddresses.filter(_.confidenceScore >= threshold).sortBy(_.underlyingScore)(Ordering[Float])
              else scoredAddresses.filter(_.confidenceScore >= threshold).sortBy(_.confidenceScore)(Ordering[Double].reverse)

            // capture the number of matches before applying offset and limit
            val newTotal = sortedAddresses.length

            // trim the result list according to offset and limit paramters
            val limitedSortedAddresses = if (!auxiliary) {
              sortedAddresses.slice(offsetInt, offsetInt + limitInt)
            } else {
              sortedAddresses.filter(_.auxiliary.isDefined).slice(offsetInt, offsetInt + round(limitInt * 0.2).toInt) ++
                sortedAddresses.filter(_.auxiliary.isEmpty).take(round(limitInt * 0.8).toInt)
            }

            // if verbose is false, strip out full address details (these are needed for score so must be
            // removed retrospectively)
            val finalAddresses = if (verb) limitedSortedAddresses else trimAddresses(limitedSortedAddresses)

            writeLog(activity = "address_request")

            jsonOk(
              AddressBySearchResponseContainer(
                apiVersion = apiVersion,
                dataVersion = dataVersion,
                response = AddressBySearchResponse(
                  tokens = tokens,
                  addresses = finalAddresses,
                  filter = filterString,
                  historical = hist,
                  epoch = epochVal,
                  rangekm = rangeVal,
                  latitude = latVal,
                  longitude = lonVal,
                  limit = limitInt,
                  offset = offsetInt,
                  total = newTotal,
                  maxScore = maxScore,
                  sampleSize = limitExpanded,
                  matchthreshold = thresholdFloat,
                  verbose = verb,
                  includeauxiliarysearch = auxiliary,
                  eboost = eboostDouble,
                  nboost = nboostDouble,
                  sboost = sboostDouble,
                  wboost = wboostDouble
                ),
                status = OkAddressResponseStatus
              )
            )
        }.recover {
          case NonFatal(exception) =>
            if (overloadProtection.breaker.isHalfOpen) {
              logger.warn(s"Elasticsearch is overloaded or down (address input). Circuit breaker is Half Open: ${exception.getMessage}")
              TooManyRequests(Json.toJson(FailedRequestToEsTooBusy(exception.getMessage, queryValues)))
            }else if (overloadProtection.breaker.isOpen) {
              logger.warn(s"Elasticsearch is overloaded or down (address input). Circuit breaker is Open: ${exception.getMessage}")
              TooManyRequests(Json.toJson(FailedRequestToEsTooBusy(exception.getMessage, queryValues)))
            } else {
              // Circuit Breaker is closed. Some other problem
              writeLog(badRequestErrorMessage = FailedRequestToEsError.message)
              logger.warn(s"Could not handle individual request (address input), problem with ES ${exception.getMessage}")
              InternalServerError(Json.toJson(FailedRequestToEs(exception.getMessage, queryValues)))
            }
        }
    }
  }
}
