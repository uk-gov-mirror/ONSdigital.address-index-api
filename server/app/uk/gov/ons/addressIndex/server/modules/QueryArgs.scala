package uk.gov.ons.addressIndex.server.modules

import uk.gov.ons.addressIndex.model.config.QueryParamsConfig
import uk.gov.ons.addressIndex.model.db.BulkAddressRequestData

import scala.util.Try

case class DateRange(start: String = "", end: String = "")

case class Region(range: Int, lat: Double, lon: Double)

object Region {
  def fromStrings(range: String, lat: String, lon: String): Option[Region] = {
    Try(Region(range.toInt, lat.toDouble, lon.toDouble)).toOption
  }
}

/** the query can be limited to only return a certain number of inputs */
trait Limitable {
  /** the maximum number of results to return */
  def limit: Int
}

object Limitable {
  val default = 10
}

/** the query can be given a starting offset from which to return results */
trait StartAtOffset {
  /** the match index at which to begin returning results */
  def start: Int
}

object StartAtOffset {
  val default = 0
}

/** the query can be filtered */
trait Filterable {
  /** */
  val filters: String

  def filtersType: String = filters match {
    case "residential" | "commercial" => "prefix"
    case f if f.endsWith("*") => "prefix"
    case _ => "term"
  }

  def filtersValuePrefix: String = filters match {
    case "residential" => "R"
    case "commercial" => "C"
    case f if f.endsWith("*") => filters.substring(0, filters.length - 1).toUpperCase
    case f => f.toUpperCase()
  }

  def filtersValueTerm: Seq[String] = filters.toUpperCase.split(",")
}

/** the query can be filtered by date */
trait DateFilterable {
  /** */
  def filterDateRange: DateRange
}

object DateFilterable {
  val default = DateRange()
}

/** the query can be told to report back in verbose mode */
trait Verboseable {
  /** */
  def verbose: Boolean
}

object Verboseable {
  val default = true
}

/** the query can be run over skinny indexes */
trait Skinnyable {
  /** */
  def skinny: Boolean
}

object Skinnyable {
  val default = false
}

/** the query takes additional configuration parameters */
trait Configurable {
  /** */
  def queryParamsConfig: Option[QueryParamsConfig]
}

object Configurable {
  val default: Option[QueryParamsConfig] = None
}

/** the query can specify which address source to use */
object AddressSource extends Enumeration {

  case class Inst(asString: String) extends super.Val {}

  def fromString(str: String): Either[String, AddressSource.Inst] = {
    str match {
      case "nionly" => Right(NIOnly)
      case "ewonly" => Right(EWOnly)
      case "niboost" => Right(NIBoost)
      case "ewboost" => Right(EWBoost)
      case "all" => Right(All)
      case _ => Left(str)
    }
  }

  val All = Inst("all")
  val NIOnly = Inst("nionly")
  val EWOnly = Inst("ewonly")
  val NIBoost = Inst("niboost")
  val EWBoost = Inst("ewboost")
}

trait FromSource {
  /** */
  def fromSource: AddressSource.Inst
}

object FromSource {
  val default: AddressSource.Inst = AddressSource.All
}

/** the root class of all query arguments */
sealed abstract class QueryArgs {
  /** */
  def epoch: String

  def epochParam: String = if (epoch.isEmpty) "_current" else "_" + epoch

  /** */
  def historical: Boolean
}

object QueryArgs {
  val epochDefault = ""
  val historicalDefault = true
}

/**
  * Search by UPRN
  *
  * @param uprn the UPRN to search by
  */
final case class UPRNArgs(uprn: String,
                          epoch: String = QueryArgs.epochDefault,
                          historical: Boolean = QueryArgs.historicalDefault,
                         ) extends QueryArgs {
}

sealed abstract class MultiResultArgs extends QueryArgs with Limitable with Filterable with Verboseable {
}

/**
  * Search according to a partial search
  *
  * @param input    the partial search to search by
  * @param fallback whether to try a slow fallback query in the event of a normal query failing
  */
final case class PartialArgs(input: String,
                             fallback: Boolean = false,
                             epoch: String = QueryArgs.epochDefault,
                             historical: Boolean = QueryArgs.historicalDefault,
                             limit: Int = Limitable.default,
                             start: Int = StartAtOffset.default,
                             filters: String,
                             filterDateRange: DateRange = DateFilterable.default,
                             verbose: Boolean = Verboseable.default,
                             skinny: Boolean = Skinnyable.default,
                             fromSource: AddressSource.Inst = FromSource.default
                            ) extends MultiResultArgs with DateFilterable with StartAtOffset with Skinnyable with FromSource {
  def inputNumbers: List[String] = input.split("\\D+").filter(_.nonEmpty).toList
}

/**
  * Search by postcode
  *
  * @param postcode the postcode to search by
  */
final case class PostcodeArgs(postcode: String,
                              epoch: String = QueryArgs.epochDefault,
                              historical: Boolean = QueryArgs.historicalDefault,
                              limit: Int = Limitable.default,
                              start: Int = StartAtOffset.default,
                              filters: String,
                              verbose: Boolean = Verboseable.default,
                              skinny: Boolean = Skinnyable.default,
                             ) extends MultiResultArgs with StartAtOffset with Skinnyable {
}

/**
  * Search at random
  */
final case class RandomArgs(epoch: String = QueryArgs.epochDefault,
                            historical: Boolean = QueryArgs.historicalDefault,
                            filters: String,
                            limit: Int = Limitable.default,
                            verbose: Boolean = Verboseable.default,
                            skinny: Boolean = Skinnyable.default,
                            fromSource: AddressSource.Inst = FromSource.default
                           ) extends MultiResultArgs with Skinnyable {
}

/**
  * Search according to a list of tokens
  *
  * @param tokens address tokens to search by
  */
final case class AddressArgs(input: String,
                             tokens: Map[String, String],
                             region: Option[Region],
                             isBulk: Boolean = false,
                             epoch: String = QueryArgs.epochDefault,
                             historical: Boolean = QueryArgs.historicalDefault,
                             limit: Int = Limitable.default,
                             start: Int = StartAtOffset.default,
                             filters: String,
                             filterDateRange: DateRange = DateFilterable.default,
                             verbose: Boolean = Verboseable.default,
                             queryParamsConfig: Option[QueryParamsConfig] = Configurable.default,
                             fromSource: AddressSource.Value = FromSource.default
                            ) extends MultiResultArgs with StartAtOffset with DateFilterable with Configurable {
}

/**
  * @param requestsData   data that will be used in the multi search request
  * @param matchThreshold required match quality, below which results are discarded
  */
final case class BulkArgs(requestsData: Stream[BulkAddressRequestData],
                          matchThreshold: Float,
                          includeFullAddress: Boolean = false,
                          epoch: String = QueryArgs.epochDefault,
                          historical: Boolean = QueryArgs.historicalDefault,
                          limit: Int = Limitable.default,
                          filterDateRange: DateRange = DateFilterable.default,
                          queryParamsConfig: Option[QueryParamsConfig] = Configurable.default,
                         ) extends QueryArgs with Limitable with DateFilterable with Configurable {
}
