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
      val parkingTimeStart = ZonedDateTime.now()

      val cases = Table(
        (
          "energy",
          "destinationPoiTyp",
          "destinationCategoricalLocation",
          "destinationPoi",
          "isChargingAtHomePossible",
          "departureTime",
          "seed",
          "expectedResult"
        ),
        (
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(14),
          seed,
          false
        ),
        (
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(29, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(51, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(39, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(86, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(29, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          true
        ),
        (
          Quantities.getQuantity(76, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          false
        ),
        (
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        ),
        (
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        ),
        (
          Quantities.getQuantity(30, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          true,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        ),
        (
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.HOME,
          CategoricalLocationDictionary.HOME,
          poiHome,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        ),
        (
          Quantities.getQuantity(40, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(85, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.WORK,
          CategoricalLocationDictionary.WORK,
          workPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        ),
        (
          Quantities.getQuantity(30, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 1
        ),
        (
          Quantities.getQuantity(75, PowerSystemUnits.KILOWATTHOUR),
          PoiTypeDictionary.SHOPPING,
          CategoricalLocationDictionary.SUPERMARKET,
          supermarketPoi,
          false,
          parkingTimeStart.plusMinutes(15),
          seed,
          seed.nextDouble() < 0
        )
      )

      forAll(cases) {
        (
            energy,
            destinationPoiTyp,
            destinationCategoricalLocation,
            destinationPoi,
            isChargingAtHomePossible,
            departureTime,
            seed,
            expectedResult
        ) =>
          val ev: ElectricVehicle = ElectricVehicle
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
              energy,
              destinationPoiTyp,
              destinationCategoricalLocation,
              destinationPoi,
              parkingTimeStart,
              departureTime
            )

          ChargingBehavior.doesEvWantToCharge(ev, seed) shouldBe expectedResult
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
  }
}
