package uk.gov.ons.addressIndex.server.controllers

import javax.inject.{Inject, Singleton}
import play.api.libs.json.Json
import play.api.mvc._
import uk.gov.ons.addressIndex.model.db.index.HybridAddressCollection
import uk.gov.ons.addressIndex.model.server.response.address.{AddressResponseAddress, FailedRequestToEsRandomError, OkAddressResponseStatus}
import uk.gov.ons.addressIndex.model.server.response.random.{AddressByRandomResponse, AddressByRandomResponseContainer}
import uk.gov.ons.addressIndex.server.model.dao.QueryValues
import uk.gov.ons.addressIndex.server.modules._
import uk.gov.ons.addressIndex.server.modules.response.RandomControllerResponse
import uk.gov.ons.addressIndex.server.modules.validation.RandomControllerValidation
import uk.gov.ons.addressIndex.server.utils.{APIThrottle, AddressAPILogger}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try
import scala.util.control.NonFatal

@Singleton
class RandomController @Inject()(val controllerComponents: ControllerComponents,
                                 esRepo: ElasticsearchRepository,
                                 conf: ConfigModule,
                                 versionProvider: VersionModule,
                                 overloadProtection: APIThrottle,
                                 randomValidation: RandomControllerValidation
                                )(implicit ec: ExecutionContext)
  extends PlayHelperController(versionProvider) with RandomControllerResponse {

  lazy val logger: AddressAPILogger = AddressAPILogger("address-index-server:RandomController")

  /**
    * Random query API
    *
    * @return Json response with addresses information
    */
  def randomQuery(classificationfilter: Option[String] = None,
                  limit: Option[String] = None,
                  historical: Option[String] = None,
                  verbose: Option[String] = None,
                  epoch: Option[String] = None,
                  fromsource: Option[String] = None
                 ): Action[AnyContent] = Action async { implicit req =>
    val startingTime = System.currentTimeMillis()

    val clusterid = conf.config.elasticSearch.clusterPolicies.random

    val defLimit = conf.config.elasticSearch.defaultLimitRandom

    val limval = limit.getOrElse(defLimit.toString)

    val filterString = classificationfilter.getOrElse("").replaceAll("\\s+", "")
    val endpointType = "random"

    val hist = historical.flatMap(x => Try(x.toBoolean).toOption).getOrElse(true)
    val verb = verbose.flatMap(x => Try(x.toBoolean).toOption).getOrElse(false)

    val epochVal = epoch.getOrElse("")
    val fromsourceVal = {if (fromsource.getOrElse("all").isEmpty) "all" else fromsource.getOrElse("all")}

    def writeLog(doResponseTime: Boolean = true, badRequestErrorMessage: String = "", notFound: Boolean = false, formattedOutput: String = "", numOfResults: String = "", score: String = "", activity: String = ""): Unit = {
      val responseTime = if (doResponseTime) (System.currentTimeMillis() - startingTime).toString else ""
      val networkid = Try(if (req.headers.get("authorization").getOrElse("Anon").indexOf("+") > 0) req.headers.get("authorization").getOrElse("Anon").split("\\+")(0) else req.headers.get("authorization").getOrElse("Anon").split("_")(0)).getOrElse("")
      val organisation = Try(if (req.headers.get("authorization").getOrElse("Anon").indexOf("+") > 0) req.headers.get("authorization").getOrElse("Anon").split("\\+")(0).split("_")(1) else "not set").getOrElse("")

      logger.systemLog(
        ip = req.remoteAddress, url = req.uri, responseTimeMillis = responseTime,
        isNotFound = notFound, filter = filterString, badRequestMessage = badRequestErrorMessage,
        limit = limval, formattedOutput = formattedOutput,
        numOfResults = numOfResults, score = score, networkid = networkid, organisation = organisation,
        historical = hist, epoch = epochVal, verbose = verb,
        endpoint = endpointType, activity = activity, clusterid = clusterid
      )
    }

    val limitInt = Try(limval.toInt).toOption.getOrElse(defLimit)

    val queryValues = QueryValues(
      epoch = Some(epochVal),
      filter = Some(filterString),
      historical = Some(hist),
      limit = Some(limitInt),
      verbose = Some(verb),
      fromsource = Some(fromsourceVal)
    )

    val result: Option[Future[Result]] =
      randomValidation.validateSource(queryValues)
        .orElse(randomValidation.validateRandomLimit(limit, queryValues))
        .orElse(randomValidation.validateKeyStatus(queryValues))
        .orElse(randomValidation.validateRandomFilter(classificationfilter, queryValues))
        .orElse(randomValidation.validateEpoch(queryValues))
        .orElse(randomValidation.validateFromSource(queryValues))
        .orElse(None)

    result match {

      case Some(res) =>
        res // a validation error

      case _ =>
        val args = RandomArgs(
          filters = filterString,
          limit = limitInt,
          historical = hist,
          verbose = verb,
          epoch = epochVal,
          skinny = !verb,
          fromsource = fromsourceVal
        )

        val request: Future[HybridAddressCollection] =
          overloadProtection.breaker.withCircuitBreaker(
            esRepo.runMultiResultQuery(args)
          )

        request.map {
          case HybridAddressCollection(hybridAddresses, _, _) =>

            val addresses: Seq[AddressResponseAddress] = hybridAddresses.map(
              AddressResponseAddress.fromHybridAddress(_, verb)
            )

            writeLog(activity = "random_address_request")

            jsonOk(
              AddressByRandomResponseContainer(
                apiVersion = apiVersion,
                dataVersion = dataVersion,
                response = AddressByRandomResponse(
                  addresses = addresses,
                  filter = filterString,
                  historical = hist,
                  epoch = epochVal,
                  limit = limitInt,
                  verbose = verb,
                  fromsource = fromsourceVal
                ),
                status = OkAddressResponseStatus
              )
            )

        }.recover {
          case NonFatal(exception) =>
            if (overloadProtection.breaker.isHalfOpen) {
              logger.warn(s"Elasticsearch is overloaded or down (random input). Circuit breaker is Half Open: ${exception.getMessage}")
              TooManyRequests(Json.toJson(FailedRequestToEsTooBusyRandom(exception.getMessage, queryValues)))
            }else if (overloadProtection.breaker.isOpen) {
              logger.warn(s"Elasticsearch is overloaded or down (random input). Circuit breaker is open: ${exception.getMessage}")
              TooManyRequests(Json.toJson(FailedRequestToEsTooBusyRandom(exception.getMessage, queryValues)))
            } else {
              // Circuit Breaker is closed. Some other problem
              writeLog(badRequestErrorMessage = FailedRequestToEsRandomError.message)
              logger.warn(s"Could not handle individual request (random input), problem with ES ${exception.getMessage}")
              InternalServerError(Json.toJson(FailedRequestToEsRandom(exception.getMessage, queryValues)))
            }
        }

    }
  }
}
