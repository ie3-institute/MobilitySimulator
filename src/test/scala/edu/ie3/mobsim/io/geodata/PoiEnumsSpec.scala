package edu.ie3.mobsim.io.geodata

import edu.ie3.mobsim.io.geodata.PoiEnums.{CategoricalLocationDictionary, PoiTypeDictionary}
import edu.ie3.mobsim.model.{ElectricVehicleTestData, TripSimulationData}
import edu.ie3.test.common.UnitSpec

import scala.util.{Failure, Success}

class PoiEnumsSpec
  extends UnitSpec
  with ElectricVehicleTestData
  with TripSimulationData {


  "PoiEnums" should {
    // testing PoiTypeDictionary.apply(token: String)
    "apply the PoiTypes correctly to the PoiTypeDictionary" in {

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

    // testing CategoricalLocationDictionary.apply(token: String)
    "apply the CategoricalLocation correctly to the CategoricalLocationDictionary" in {

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
        ("charginghubhighway", CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY)
      )

      forAll(cases) { (categoricalLocation, expectedResult) =>
        CategoricalLocationDictionary.apply(categoricalLocation) shouldBe expectedResult
      }
    }

    // testing CategoricalLocationDictionary.apply(poiType: PoiTypeDictionary.Value)
    "apply the PoiTypes correctly to the CategoricalLocationDictionary" in {

      val cases = Table(
        ("poiType", "expectedResult"),
        (PoiTypeDictionary.HOME, CategoricalLocationDictionary.HOME),
        (PoiTypeDictionary.WORK, CategoricalLocationDictionary.WORK),
        (PoiTypeDictionary.CHARGING_HUB_TOWN, CategoricalLocationDictionary.CHARGING_HUB_TOWN),
        (PoiTypeDictionary.CHARGING_HUB_HIGHWAY, CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY)
      )

      forAll(cases) { (poiType, expectedResult) =>
        CategoricalLocationDictionary.apply(poiType) shouldBe expectedResult
      }
    }
  }
}
