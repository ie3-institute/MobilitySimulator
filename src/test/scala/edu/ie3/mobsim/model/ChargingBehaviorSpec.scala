/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary.HOME
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.mutable

class ChargingBehaviorSpec extends UnitSpec with ChargingBehaviorTestData {
  "The ChargingBehavior" should {

    "choose a chargingStation if charging is needed" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        evChargingNeeded,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        random,
        maxDistance
      )

      uuid shouldBe Some(cs2.getUuid)
    }

    "choose no chargingStation if charging is not needed" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        ev1,
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        random,
        maxDistance
      )

      uuid shouldBe None
    }

    "choose no chargingStation if no station is nearby" in {
      val uuid: Option[UUID] = ChargingBehavior.chooseChargingStation(
        evNoChargingStations,
        currentPricesAtChargingStations,
        noAvailableChargingPoints,
        random,
        maxDistance
      )

      uuid shouldBe None
    }

    "check if ev wants to charge" in {
      val parkingTimeStart = ZonedDateTime.now()

      val cases = Table(
        (
          "soc",
          "destinationPoiTyp",
          "destinationPoi",
          "isChargingAtHomePossible",
          "departureTime",
          "expectedResult"
        ),
        (
          // EV with destination home and home charging possible stays
          // not long enough -> does not want to charge
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(14),
          false
        ),
        (
          // EV with destination home and home charging possible has
          // soc under lower threshold and stays long enough -> wants to charge
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination home and home charging possible has
          // soc over upper threshold -> does not want to charge
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination work and home charging possible stays long enough and
          // is under lower threshold -> wants to charge
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination work and home charging possible and stays long enough
          // is over upper threshold -> does not want to charge
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination supermarket and home charging possible has
          // soc under lower threshold -> wants to charge
          Quantities.getQuantity(29, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination supermarket and home charging possible has
          // soc over upper threshold -> does not want to charge
          Quantities.getQuantity(51, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination home and home charging not possible has
          // soc has soc under lower threshold -> wants to charge
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination home and home charging not possible has
          // soc over upper threshold -> does not want to charge
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination work and home charging not possible has
          // soc under lower threshold -> wants to charge
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination work and home charging not possible has
          // soc over upper threshold -> does not want to charge
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination supermarket and home charging not possible has
          // soc under lower threshold -> wants to charge
          Quantities.getQuantity(29, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination supermarket and home charging not possible has
          // soc over upper threshold -> does not want to charge
          Quantities.getQuantity(76, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination home and home charging possible has
          // soc = lower thresholds -> wants to charge
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination home and home charging possible has
          // soc = upper thresholds -> does not want to charge
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination work and home charging possible has
          // soc = lower thresholds -> wants to charge
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination work and home charging possible has
          // soc = upper thresholds -> does not want to charge
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination supermarket and home charging possible has
          // soc = lower thresholds -> wants to charge
          Quantities.getQuantity(30, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination supermarket and home charging possible has
          // soc = upper thresholds -> does not want to charge
          Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination home and home charging not possible has
          // soc = lower threshold -> wants to charge
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination home and home charging not possible has
          // soc = upper threshold -> does not want to charge
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination work and home charging not possible has
          // soc = lower threshold -> wants to charge
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination work and home charging not possible has
          // soc = upper threshold -> does not want to charge
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        ),
        (
          // EV with destination supermarket and home charging not possible has
          // soc = lower threshold -> wants to charge
          Quantities.getQuantity(30, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          true
        ),
        (
          // EV with destination supermarket and home charging not possible has
          // soc = upper threshold -> does not want to charge
          Quantities.getQuantity(75, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          false
        )
      )

      forAll(cases) {
        (
            soc,
            destinationPoiTyp,
            destinationPoi,
            isChargingAtHomePossible,
            departureTime,
            expectedResult
        ) =>
          val ev = ElectricVehicle
            .buildEv(
              "car",
              givenModel,
              givenHomePoi,
              givenWorkPoi,
              givenSimulationStart,
              givenFirstDeparture,
              isChargingAtHomePossible
            )
            .copyWith(
              soc,
              destinationPoiTyp,
              destinationPoi.categoricalLocation,
              destinationPoi,
              parkingTimeStart,
              departureTime
            )

          ChargingBehavior.doesEvWantToCharge(
            ev,
            random
          ) shouldBe expectedResult
      }
    }

    "give correct rations for chargingStations" in {
      val x = ChargingBehavior.createRatingsForChargingStations(
        evChargingNeeded.copyWith(half),
        currentPricesAtChargingStations,
        currentlyAvailableChargingPoints,
        maxDistance
      )

      x shouldBe Map(
        UUID.fromString("7537c0b6-3137-4e30-8a95-db1c0f9d9b81") -> 5.0
      )
    }

    "return the correct price rating" in {
      val priceRating = PrivateMethod[Double](Symbol("priceRating"))

      val evWithLowPriceMemory: ElectricVehicle = ev1
      evWithLowPriceMemory.updateChargingPricesMemory(0.0)

      val evWithHighPriceMemory: ElectricVehicle = ev1
      evWithHighPriceMemory.updateChargingPricesMemory(0.0)
      evWithHighPriceMemory.updateChargingPricesMemory(1.0)

      val evOtherPriceMemory: ElectricVehicle = ev1
      evOtherPriceMemory.updateChargingPricesMemory(0.5)
      evOtherPriceMemory.updateChargingPricesMemory(0.7)
      evOtherPriceMemory.updateChargingPricesMemory(0.3)

      val cases = Table(
        ("ev", "prices", "expectedRating"),
        (ev1, Map.empty[UUID, Double], 1.0),
        (ev1, Map(cs6.getUuid -> 0.0), 1.0),
        (evWithLowPriceMemory, Map(cs6.getUuid -> 0.0), 0.5),
        (evWithHighPriceMemory, Map(cs6.getUuid -> 0.0), 1.0),
        (evOtherPriceMemory, Map(cs6.getUuid -> 0.6), 0.25),
        (evOtherPriceMemory, Map(cs6.getUuid -> 0.4), 0.75)
      )

      forAll(cases) { (ev, prices, expectedRating) =>
        val result = ChargingBehavior invokePrivate priceRating(
          cs6,
          ev,
          prices
        )
        assert(result === expectedRating +- 1e-6)
      }
    }
  }
}
