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
import edu.ie3.test.common.UnitSpec

class PoiEnumsSpec extends UnitSpec {

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
        ("charging_hub_town", PoiTypeDictionary.CHARGING_HUB_TOWN),
        ("charging_hub_highway", PoiTypeDictionary.CHARGING_HUB_HIGHWAY)
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

      exception.getMessage shouldBe "PoiType 'wrong_poi' not known"
    }

    // testing CategoricalLocationDictionary.apply(token: String)
    "parse CategoricalLocation correctly" in {
      val cases = Table(
        ("categoricalLocation", "expectedResult"),
        ("home", CategoricalLocationDictionary.HOME),
        ("work", CategoricalLocationDictionary.WORK),
        ("supermarket", CategoricalLocationDictionary.SUPERMARKET),
        ("services", CategoricalLocationDictionary.SERVICES),
        ("other_shop", CategoricalLocationDictionary.OTHER_SHOP),
        ("medicinal", CategoricalLocationDictionary.MEDICINAL),
        ("bbpg", CategoricalLocationDictionary.BBPG),
        ("restaurant", CategoricalLocationDictionary.RESTAURANT),
        ("culture", CategoricalLocationDictionary.CULTURE),
        ("sports", CategoricalLocationDictionary.SPORTS),
        ("religious", CategoricalLocationDictionary.RELIGIOUS),
        ("charging_hub_town", CategoricalLocationDictionary.CHARGING_HUB_TOWN),
        (
          "charging_hub_highway",
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
