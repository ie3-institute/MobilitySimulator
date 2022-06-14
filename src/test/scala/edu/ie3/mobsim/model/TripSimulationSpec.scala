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
import edu.ie3.mobsim.utils.IoUtils
import edu.ie3.test.common.UnitSpec
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.METRE

import scala.collection.mutable

class TripSimulationSpec
    extends UnitSpec
    with ElectricVehicleTestData
    with ChargingBehaviorTestData {

  "The TripSimulation" should {
    "simulate next trip correctly" in {
      val x = TripSimulation.simulateNextTrip(
        givenSimulationStart,
        ev.copyWith(half),
        poisWithSizes,
        chargingHubTownIsPresent = true,
        chargingHubHighwayIsPresent = true,
        chargingStations,
        ioUtils,
        categoricalLocation,
        speed,
        firstDepartureOfDay,
        lastTripOfDay,
        parkingTime,
        poiTransition,
        tripDistance,
        maxDistance
      )

      x match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              model,
              batteryCapacity,
              acChargingPower,
              dcChargingPower,
              consumption,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoiType,
              destinationCategoricalLocation,
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoiType,
              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart

      }

    }

    // testing makeTripToChargingHub
    "makeTripToChargingHub correctly" in {

      TripSimulation.makeTripToChargingHub(
        "charginghubtown",
        ev,
        givenSimulationStart,
        poisWithSizes,
        0.5,
        0.2,
        maxDistance,
        plannedDestinationPoi,
        1,
        chargingStations,
        speed
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              model,
              batteryCapacity,
              acChargingPower,
              dcChargingPower,
              consumption,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoiType,
              destinationCategoricalLocation,
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoiType,
              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          id shouldBe "test_car"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN.id
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN.id
          destinationPoi shouldBe plannedDestinationPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(10)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(26)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe Some(PoiTypeDictionary.WORK.id)
          finalDestinationPoi shouldBe Some(plannedDestinationPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(-7000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    // testing makeModifiedTripToChargingHub
    "makeModifiedTripToChargingHub correctly" in {

      TripSimulation.makeModifiedTripToChargingHub(
        "charginghubtown",
        ev,
        givenSimulationStart,
        poisWithSizes,
        0.2,
        maxDistance,
        plannedDestinationPoi,
        1,
        chargingStations,
        speed
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              model,
              batteryCapacity,
              acChargingPower,
              dcChargingPower,
              consumption,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoiType,
              destinationCategoricalLocation,
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoiType,
              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          id shouldBe "test_car"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN.id
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN.id
          destinationPoi shouldBe plannedDestinationPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(17)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe Some(PoiTypeDictionary.WORK.id)
          finalDestinationPoi shouldBe Some(plannedDestinationPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(10000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }
  }
}
