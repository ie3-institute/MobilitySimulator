/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.test.common.UnitSpec

import java.util.UUID

class ChargingBehaviorSpec extends UnitSpec with ChargingBehaviorTestData {
  "The ChargingBehavior" should {

    "choose a chargingStation if charging is needed" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        seed,
        maxDistance
      )

      uuid.isDefined shouldBe true
      uuid.map(_.toString) shouldBe Some("7537c0b6-3137-4e30-8a95-db1c0f9d9b81")
    }

    "choose no chargingStation if charging is not needed" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        ev1.copyWith(ev1.getEStorage),
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        seed,
        maxDistance
      )

      uuid.isDefined shouldBe false
    }

    "choose no chargingStation if no station is nearby" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        evNoChargingStations,
        currentPricesAtChargingStations,
        noAvailableChargingPoints,
        seed,
        maxDistance
      )

      uuid.isDefined shouldBe false
    }

    "check if ev wants to charge" in {
      ChargingBehavior.doesEvWantToCharge(
        evChargingNeeded.copyWith(zero),
        seed
      ) shouldBe true
      ChargingBehavior.doesEvWantToCharge(
        ev1.copyWith(ev1.getEStorage),
        seed
      ) shouldBe false
    }

    "give correct rations for chargingStations" in {
      val x = ChargingBehavior.createRatingsForChargingStations(
        ev1.copyWith(half),
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        maxDistance
      )

      x shouldBe Map(
        UUID.fromString("7537c0b6-3137-4e30-8a95-db1c0f9d9b81") -> 4.0
      )
    }
  }
}
