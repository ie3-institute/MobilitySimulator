/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.PoiTransition.PoiTransitionKey
import edu.ie3.test.common.UnitSpec

class PoiTransitionFactorySpec extends UnitSpec {

  "Build the correct poi transition pdf" in {

    val actual = PoiTransitionFactory
      .getFromFile(
        "/Users/thomas/IdeaProjects/MobilitySimulator/input/mobilitySimulator/trip_probabilities/transition.csv",
        ","
      )
      .getOrElse(fail("Unable to read file"))

    val poiTransitionKeyHome = PoiTransitionKey(0, PoiTypeDictionary.HOME)
    val probabilitiesHome = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyHome,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesHome.pdf.get(0) shouldBe Some(0.1)
    probabilitiesHome.pdf.get(1) shouldBe Some(0.2)
    probabilitiesHome.pdf.get(2) shouldBe Some(0.3)
    probabilitiesHome.pdf.get(3) shouldBe Some(0.4)
    probabilitiesHome.pdf.get(4) shouldBe Some(0.5)
    probabilitiesHome.pdf.get(5) shouldBe Some(0.6)
    probabilitiesHome.pdf.get(6) shouldBe Some(0.7)

    val poiTransitionKeyWork = PoiTransitionKey(0, PoiTypeDictionary.WORK)
    val probabilitiesWork = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyWork,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesWork.pdf.get(0) shouldBe Some(0.11)
    probabilitiesWork.pdf.get(1) shouldBe Some(0.22)
    probabilitiesWork.pdf.get(2) shouldBe Some(0.33)
    probabilitiesWork.pdf.get(3) shouldBe Some(0.44)
    probabilitiesWork.pdf.get(4) shouldBe Some(0.55)
    probabilitiesWork.pdf.get(5) shouldBe Some(0.66)
    probabilitiesWork.pdf.get(6) shouldBe Some(0.77)

    val poiTransitionKeyShopping =
      PoiTransitionKey(0, PoiTypeDictionary.SHOPPING)
    val probabilitiesShopping = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyShopping,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesShopping.pdf.get(0) shouldBe Some(0.12)
    probabilitiesShopping.pdf.get(1) shouldBe Some(0.22)
    probabilitiesShopping.pdf.get(2) shouldBe Some(0.32)
    probabilitiesShopping.pdf.get(3) shouldBe Some(0.42)
    probabilitiesShopping.pdf.get(4) shouldBe Some(0.52)
    probabilitiesShopping.pdf.get(5) shouldBe Some(0.62)
    probabilitiesShopping.pdf.get(6) shouldBe Some(0.72)

    val poiTransitionKeyLeisure = PoiTransitionKey(0, PoiTypeDictionary.LEISURE)
    val probabilitiesLeisure = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyLeisure,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesLeisure.pdf.get(0) shouldBe Some(0.13)
    probabilitiesLeisure.pdf.get(1) shouldBe Some(0.23)
    probabilitiesLeisure.pdf.get(2) shouldBe Some(0.33)
    probabilitiesLeisure.pdf.get(3) shouldBe Some(0.43)
    probabilitiesLeisure.pdf.get(4) shouldBe Some(0.53)
    probabilitiesLeisure.pdf.get(5) shouldBe Some(0.63)
    probabilitiesLeisure.pdf.get(6) shouldBe Some(0.73)

    val poiTransitionKeyOther = PoiTransitionKey(0, PoiTypeDictionary.OTHER)
    val probabilitiesOther = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyOther,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesOther.pdf.get(0) shouldBe Some(0.14)
    probabilitiesOther.pdf.get(1) shouldBe Some(0.24)
    probabilitiesOther.pdf.get(2) shouldBe Some(0.34)
    probabilitiesOther.pdf.get(3) shouldBe Some(0.44)
    probabilitiesOther.pdf.get(4) shouldBe Some(0.54)
    probabilitiesOther.pdf.get(5) shouldBe Some(0.64)
    probabilitiesOther.pdf.get(6) shouldBe Some(0.74)

    val poiTransitionKeyCht =
      PoiTransitionKey(0, PoiTypeDictionary.CHARGING_HUB_TOWN)
    val probabilitiesCht = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyCht,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesCht.pdf.get(0) shouldBe Some(0.15)
    probabilitiesCht.pdf.get(1) shouldBe Some(0.25)
    probabilitiesCht.pdf.get(2) shouldBe Some(0.35)
    probabilitiesCht.pdf.get(3) shouldBe Some(0.45)
    probabilitiesCht.pdf.get(4) shouldBe Some(0.55)
    probabilitiesCht.pdf.get(5) shouldBe Some(0.65)
    probabilitiesCht.pdf.get(6) shouldBe Some(0.75)

    val poiTransitionKeyChh =
      PoiTransitionKey(0, PoiTypeDictionary.CHARGING_HUB_HIGHWAY)
    val probabilitiesChh = actual.probabilitiesWeekday.getOrElse(
      poiTransitionKeyChh,
      fail(s"Could not find the probabilities for $poiTransitionKeyHome")
    )
    probabilitiesChh.pdf.get(0) shouldBe Some(0.16)
    probabilitiesChh.pdf.get(1) shouldBe Some(0.26)
    probabilitiesChh.pdf.get(2) shouldBe Some(0.36)
    probabilitiesChh.pdf.get(3) shouldBe Some(0.46)
    probabilitiesChh.pdf.get(4) shouldBe Some(0.56)
    probabilitiesChh.pdf.get(5) shouldBe Some(0.66)
    probabilitiesChh.pdf.get(6) shouldBe Some(0.76)
  }
}
