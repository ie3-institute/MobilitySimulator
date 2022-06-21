/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.model.{ElectricVehicleTestData, TripSimulationData}
import edu.ie3.test.common.UnitSpec

class PoiEnumsSpec
    extends UnitSpec
    with ElectricVehicleTestData
    with TripSimulationData {

  "PoiEnums" should {
    // testing PoiTypeDictionary.apply(token: String)
    "parse PoiTypes correctly" in {

      val cases = Table(
        ("poiType", "expectedResult"),
        ("home", PoiTypeDictionary.HOME),
        ("work", PoiTypeDictionary.WORK),
        ("shopping", PoiTypeDictionary.SHOPPING),
        ("leisure", PoiTypeDictionary.LEISURE),
        ("other", PoiTypeDictionary.OTHER),
        ("charginghubtown", PoiTypeDictionary.CHARGING_HUB_TOWN),
        ("charginghubhighway", PoiTypeDictionary.CHARGING_HUB_HIGHWAY)
      )

      forAll(cases) { (poiType, expectedResult) =>
        PoiTypeDictionary.apply(poiType) shouldBe expectedResult
      }
    }

    // parsing malformed string to PoiTypeDictionary
    "throw error if malformed string is parsed to PoiTypeDictionary" in {
      val exception = intercept[RuntimeException] {
        PoiTypeDictionary.apply("wrong_poi")
      }

      exception.getMessage shouldBe "POI not known"
    }

    // testing PoiTypeDictionary.apply(categoricalLocation: CategoricalLocationDictionary.Value)
    "convert CategoricalLocations to PoiTypeDictionary" in {

      val cases = Table(
        ("categoricalLocation", "expectedResult"),
        (CategoricalLocationDictionary.HOME, PoiTypeDictionary.HOME),
        (CategoricalLocationDictionary.WORK, PoiTypeDictionary.WORK),
        (CategoricalLocationDictionary.SUPERMARKET, PoiTypeDictionary.SHOPPING),
        (CategoricalLocationDictionary.SERVICES, PoiTypeDictionary.SHOPPING),
        (CategoricalLocationDictionary.OTHER_SHOP, PoiTypeDictionary.SHOPPING),
        (CategoricalLocationDictionary.RESTAURANT, PoiTypeDictionary.LEISURE),
        (CategoricalLocationDictionary.CULTURE, PoiTypeDictionary.LEISURE),
        (CategoricalLocationDictionary.SPORTS, PoiTypeDictionary.LEISURE),
        (CategoricalLocationDictionary.RELIGIOUS, PoiTypeDictionary.LEISURE),
        (CategoricalLocationDictionary.MEDICINAL, PoiTypeDictionary.OTHER),
        (CategoricalLocationDictionary.BBPG, PoiTypeDictionary.OTHER),
        (
          CategoricalLocationDictionary.CHARGING_HUB_TOWN,
          PoiTypeDictionary.CHARGING_HUB_TOWN
        ),
        (
          CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY,
          PoiTypeDictionary.CHARGING_HUB_HIGHWAY
        )
      )

      forAll(cases) { (categoricalLocation, expectedResult) =>
        PoiTypeDictionary.apply(categoricalLocation) shouldBe expectedResult
      }
    }

    // testing CategoricalLocationDictionary.apply(token: String)
    "parse CategoricalLocation correctly" in {

      val cases = Table(
        ("categoricalLocation", "expectedResult"),
        ("home", CategoricalLocationDictionary.HOME),
        ("work", CategoricalLocationDictionary.WORK),
        ("supermarket", CategoricalLocationDictionary.SUPERMARKET),
        ("services", CategoricalLocationDictionary.SERVICES),
        ("othershop", CategoricalLocationDictionary.OTHER_SHOP),
        ("medicinal", CategoricalLocationDictionary.MEDICINAL),
        ("bbpg", CategoricalLocationDictionary.BBPG),
        ("restaurant", CategoricalLocationDictionary.RESTAURANT),
        ("culture", CategoricalLocationDictionary.CULTURE),
        ("sports", CategoricalLocationDictionary.SPORTS),
        ("religious", CategoricalLocationDictionary.RELIGIOUS),
        ("charginghubtown", CategoricalLocationDictionary.CHARGING_HUB_TOWN),
        (
          "charginghubhighway",
          CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY
        )
      )

      forAll(cases) { (categoricalLocation, expectedResult) =>
        CategoricalLocationDictionary.apply(
          categoricalLocation
        ) shouldBe expectedResult
      }
    }

    // parsing malformed string to CategoricalLocationDictionary
    "throw error if malformed string is parsed to CategoricalLocationDictionary" in {
      val exception = intercept[RuntimeException] {
        CategoricalLocationDictionary.apply("wrong_poi")
      }

      exception.getMessage shouldBe "CategoricalLocation 'wrong_poi' not known"
    }

    // testing CategoricalLocationDictionary.apply(poiType: PoiTypeDictionary.Value)
    "convert PoiTypes to CategoricalLocationDictionary" in {

      val cases = Table(
        ("poiType", "expectedResult"),
        (PoiTypeDictionary.HOME, CategoricalLocationDictionary.HOME),
        (PoiTypeDictionary.WORK, CategoricalLocationDictionary.WORK),
        (
          PoiTypeDictionary.CHARGING_HUB_TOWN,
          CategoricalLocationDictionary.CHARGING_HUB_TOWN
        ),
        (
          PoiTypeDictionary.CHARGING_HUB_HIGHWAY,
          CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY
        )
      )

      forAll(cases) { (poiType, expectedResult) =>
        CategoricalLocationDictionary.apply(poiType) shouldBe expectedResult
      }
    }

    // parsing wrong poiType to CategoricalLocationDictionary
    "throw error if wrong PoiType is parsed to CategoricalLocationDictionary" in {
      val exception = intercept[RuntimeException] {
        CategoricalLocationDictionary.apply(PoiTypeDictionary.OTHER)
      }

      exception.getMessage shouldBe "PoiType 'OTHER' could not be applied to CategoricalLocationDictionary"
    }
  }
}
