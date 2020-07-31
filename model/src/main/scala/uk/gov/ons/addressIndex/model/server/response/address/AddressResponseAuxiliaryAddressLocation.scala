package uk.gov.ons.addressIndex.model.server.response.address

import play.api.libs.json.{Format, Json}

  /**
    * Auxiliary Address Location DTO
    */
  case class AddressResponseAuxiliaryAddressLocation(
                                       lat: String,
                                       lon: String
                                     )

  object AddressResponseAuxiliaryAddressLocation {

    implicit lazy val addressResponseAuxiliaryAddressLocationFormat: Format[AddressResponseAuxiliaryAddressLocation] = Json.format[AddressResponseAuxiliaryAddressLocation]

    object Fields {
      /**
        * Document Fields
        */
      val lat: String = "lat"
      val lon: String = "lon"
    }
  }



