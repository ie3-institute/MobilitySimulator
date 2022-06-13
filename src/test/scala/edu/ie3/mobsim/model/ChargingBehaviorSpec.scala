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
      val uuid: UUID = ChargingBehavior.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        seed,
        maxDistance
      ) match {
        case Some(uuid) => uuid
        case None => UUID.fromString("c34a031e-8368-4b59-99e4-1ad283bef6ea")
      }

      uuid.toString shouldBe "7537c0b6-3137-4e30-8a95-db1c0f9d9b81"
    }

    "choose no chargingStation if charging is not needed" in {
      val uuid: UUID = ChargingBehavior.chooseChargingStation(
        ev.copyWith(ev.getEStorage),
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        seed,
        maxDistance
      ) match {
        case Some(uuid) => uuid
        case None => UUID.fromString("c34a031e-8368-4b59-99e4-1ad283bef6ea")
      }

      uuid.toString shouldBe "c34a031e-8368-4b59-99e4-1ad283bef6ea"
    }

    "choose no chargingStation if no station is nearby" in {
      val uuid: UUID = ChargingBehavior.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations,
        noAvailableChargingPoints,
        seed,
        maxDistance
      ) match {
        case Some(uuid) => uuid
        case None => UUID.fromString("c34a031e-8368-4b59-99e4-1ad283bef6ea")
      }

      uuid.toString shouldBe "c34a031e-8368-4b59-99e4-1ad283bef6ea"
    }

    "should check if ev wants to charge" in {
      ChargingBehavior.doesEvWantToCharge(
        evChargingNeeded.copyWith(zero),
        seed
      ) shouldBe true
      ChargingBehavior.doesEvWantToCharge(
        ev.copyWith(ev.getEStorage),
        seed
      ) shouldBe false
    }

    "should give correct rations for chargingStations" in {
      val x = ChargingBehavior.createRatingsForChargingStations(
        ev.copyWith(half),
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
