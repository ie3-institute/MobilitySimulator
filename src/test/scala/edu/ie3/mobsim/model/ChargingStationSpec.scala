/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.geodata.PoiTestData
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.Queue

class ChargingStationSpec
    extends UnitSpec
    with ChargingStationTestData
    with PoiTestData {
  "The ChargingStation" should {

    "choose a chargingStation if charging is needed" in {
      val (uuid, evOption) = ChargingStation.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        random,
        maxDistance,
      )

      uuid shouldBe Some(cs2.uuid)
      evOption shouldBe Some(
        evChargingNeeded.updateChargingPricesMemory(Queue(2.0, 10.0))
      )
    }

    "choose a chargingStation if charging is needed plus check for correct rating" in {
      val (uuid, evOption) = ChargingStation.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations.updated(cs7.uuid, 0.0),
        currentlyAvailableChargingPoints,
        random,
        maxDistance,
      )

      uuid shouldBe Some(cs7.uuid)
      evOption shouldBe Some(
        evChargingNeeded.updateChargingPricesMemory(Queue(2.0, 0.0))
      )
    }

    "choose no chargingStation if charging is not needed" in {
      val (uuid, evOption) = ChargingStation.chooseChargingStation(
        ev1,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        random,
        maxDistance,
      )

      uuid shouldBe None
      evOption shouldBe None
    }

    "choose no chargingStation if no station is nearby" in {
      val (uuid, evOption) = ChargingStation.chooseChargingStation(
        evNoChargingStations,
        currentPricesAtChargingStations,
        noAvailableChargingPoints,
        random,
        maxDistance,
      )

      uuid shouldBe None
      evOption shouldBe None
    }

    "give correct rations for chargingStations" in {
      val x = ChargingStation.createRatingsForChargingStations(
        evChargingNeeded.copyWith(half),
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        maxDistance,
      )

      x shouldBe Map(
        UUID.fromString("7537c0b6-3137-4e30-8a95-db1c0f9d9b81") -> 5.0,
        cs7.uuid -> 5.0,
      )
    }

    "return the correct price rating" in {
      val priceRating = PrivateMethod[Double](Symbol("priceRating"))
      val queue = Queue(0.0)

      val evWithLowPriceMemory: ElectricVehicle =
        ev1.updateChargingPricesMemory(queue)
      val evWithHighPriceMemory: ElectricVehicle =
        ev1.updateChargingPricesMemory(queue :+ 1.0)
      val evOtherPriceMemory: ElectricVehicle =
        ev1.updateChargingPricesMemory(Queue(0.5, 0.7, 0.3))

      val cases = Table(
        ("ev", "prices", "expectedRating"),
        (ev1, Map.empty[UUID, Double], 1.0),
        (ev1, Map(cs6.uuid -> 0.0), 1.0),
        (evWithLowPriceMemory, Map(cs6.uuid -> 0.0), 0.5),
        (evWithHighPriceMemory, Map(cs6.uuid -> 0.0), 1.0),
        (evOtherPriceMemory, Map(cs6.uuid -> 0.6), 0.25),
        (evOtherPriceMemory, Map(cs6.uuid -> 0.4), 0.75),
      )

      forAll(cases) { (ev, prices, expectedRating) =>
        val result = ChargingStation invokePrivate priceRating(
          cs6,
          ev,
          prices,
        )
        assert(result === expectedRating +- 1e-6)
      }
    }
  }
}
