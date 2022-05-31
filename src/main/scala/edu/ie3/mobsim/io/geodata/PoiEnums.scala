/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

object PoiEnums {

  object PoiTypeDictionary extends Enumeration {

    val HOME, WORK, SHOPPING, LEISURE, OTHER, CHARGING_HUB_TOWN,
        CHARGING_HUB_HIGHWAY = Value

    def apply(token: String): Value = {
      token.toLowerCase match {
        case "home"               => HOME
        case "work"               => WORK
        case "shopping"           => SHOPPING
        case "leisure"            => LEISURE
        case "other"              => OTHER
        case "charginghubtown"    => CHARGING_HUB_TOWN
        case "charginghubhighway" => CHARGING_HUB_HIGHWAY
        case _                    => throw new RuntimeException("POI not known")
      }
    }
  }

  object CategoricalLocationDictionary extends Enumeration {

    val HOME, WORK, SUPERMARKET, SERVICES, OTHER_SHOP, MEDICINAL, BBPG,
        RESTAURANT, CULTURE, SPORTS, RELIGIOUS, CHARGING_HUB_TOWN,
        CHARGING_HUB_HIGHWAY = Value

    def apply(token: String): Value = {
      token.toLowerCase match {
        case "home"               => HOME
        case "work"               => WORK
        case "supermarket"        => SUPERMARKET
        case "services"           => SERVICES
        case "othershop"          => OTHER_SHOP
        case "medicinal"          => MEDICINAL
        case "bbpg"               => BBPG
        case "restaurant"         => RESTAURANT
        case "culture"            => CULTURE
        case "sports"             => SPORTS
        case "religious"          => RELIGIOUS
        case "charginghubtown"    => CHARGING_HUB_TOWN
        case "charginghubhighway" => CHARGING_HUB_HIGHWAY
        case malformed =>
          throw new RuntimeException(
            s"CategoricalLocation '$malformed' not known"
          )
      }
    }

    def apply(poiType: PoiTypeDictionary.Value): Value = {
      apply(poiType)
    }
  }

}
