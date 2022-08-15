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
        case "home"                 => HOME
        case "work"                 => WORK
        case "shopping"             => SHOPPING
        case "leisure"              => LEISURE
        case "other"                => OTHER
        case "charging_hub_town"    => CHARGING_HUB_TOWN
        case "charging_hub_highway" => CHARGING_HUB_HIGHWAY
        case malformed =>
          throw new RuntimeException(s"PoiType '$malformed' not known")
      }
    }

    def apply(
        categoricalLocation: CategoricalLocationDictionary.Value
    ): Value = {
      categoricalLocation match {
        case CategoricalLocationDictionary.HOME        => HOME
        case CategoricalLocationDictionary.WORK        => WORK
        case CategoricalLocationDictionary.SUPERMARKET => SHOPPING
        case CategoricalLocationDictionary.SERVICES    => SHOPPING
        case CategoricalLocationDictionary.OTHER_SHOP  => SHOPPING
        case CategoricalLocationDictionary.RESTAURANT  => LEISURE
        case CategoricalLocationDictionary.CULTURE     => LEISURE
        case CategoricalLocationDictionary.SPORTS      => LEISURE
        case CategoricalLocationDictionary.RELIGIOUS   => LEISURE
        case CategoricalLocationDictionary.MEDICINAL   => OTHER
        case CategoricalLocationDictionary.BBPG        => OTHER
        case CategoricalLocationDictionary.CHARGING_HUB_TOWN =>
          CHARGING_HUB_TOWN
        case CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY =>
          CHARGING_HUB_HIGHWAY
      }
    }
  }

  object CategoricalLocationDictionary extends Enumeration {

    val HOME, WORK, SUPERMARKET, SERVICES, OTHER_SHOP, MEDICINAL, BBPG,
        RESTAURANT, CULTURE, SPORTS, RELIGIOUS, CHARGING_HUB_TOWN,
        CHARGING_HUB_HIGHWAY = Value

    def apply(token: String): Value = {
      token.toLowerCase match {
        case "home"                 => HOME
        case "work"                 => WORK
        case "supermarket"          => SUPERMARKET
        case "services"             => SERVICES
        case "other_shop"           => OTHER_SHOP
        case "medicinal"            => MEDICINAL
        case "bbpg"                 => BBPG
        case "restaurant"           => RESTAURANT
        case "culture"              => CULTURE
        case "sports"               => SPORTS
        case "religious"            => RELIGIOUS
        case "charging_hub_town"    => CHARGING_HUB_TOWN
        case "charging_hub_highway" => CHARGING_HUB_HIGHWAY
        case malformed =>
          throw new RuntimeException(
            s"CategoricalLocation '$malformed' not known"
          )
      }
    }

    def apply(poiType: PoiTypeDictionary.Value): Value = {
      poiType match {
        case PoiTypeDictionary.HOME                 => HOME
        case PoiTypeDictionary.WORK                 => WORK
        case PoiTypeDictionary.CHARGING_HUB_TOWN    => CHARGING_HUB_TOWN
        case PoiTypeDictionary.CHARGING_HUB_HIGHWAY => CHARGING_HUB_HIGHWAY
        case malformed =>
          throw new RuntimeException(
            s"PoiType '$malformed' could not be applied to CategoricalLocationDictionary"
          )
      }
    }
  }

}
