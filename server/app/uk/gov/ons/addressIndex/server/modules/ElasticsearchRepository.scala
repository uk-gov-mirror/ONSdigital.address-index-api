package uk.gov.ons.addressIndex.server.modules

import com.google.inject.ImplementedBy
import com.sksamuel.elastic4s.requests.searches.SearchRequest
import uk.gov.ons.addressIndex.model.db.BulkAddressRequestData
import uk.gov.ons.addressIndex.model.db.index._
import uk.gov.ons.addressIndex.model.server.response.bulk.AddressBulkResponseAddress

import scala.concurrent.Future

@ImplementedBy(classOf[AddressIndexRepository])
trait ElasticsearchRepository {

  /**
    * Queries if ES is up
    */
  def queryHealth(): Future[String]

  /**
    * Generates the ES request to get addresses from ES
    * Supports all of the different query types
    * Public so that it can be accessed by the debug controller, and tests
    *
    * @param args arguments for the ES query
    * @return Search definition describing an ES query
    */
  def makeQuery(args: QueryArgs): SearchRequest

  /**
    * Query the address index by UPRN.
    *
    * @param args the query arguments, including the identificator of the address
    * @return Future containing a address or `None` if not in the index
    */
  def runUPRNQuery(args: UPRNArgs): Future[Option[HybridAddress]]

  /**
    * Query the address index by partial address, randomness, postcode, or full address.
    *
    * @param args the query arguments
    * @return Future containing a collection of addresses
    */
  def runMultiResultQuery(args: MultiResultArgs): Future[HybridAddressCollection]

  /**
    * Query ES using MultiSearch endpoint
    *
    * @param args bulk query arguments
    * @return a stream of `Either`, `Right` will contain resulting bulk address,
    *         `Left` will contain request data that is to be re-send
    */
  def runBulkQuery(args: BulkArgs): Future[Stream[Either[BulkAddressRequestData, Seq[AddressBulkResponseAddress]]]]
}
