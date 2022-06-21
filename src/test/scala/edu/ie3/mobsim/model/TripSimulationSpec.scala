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
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.METRE

import scala.collection.mutable

class TripSimulationSpec extends UnitSpec with ChargingBehaviorTestData {

  "TripSimulation" should {
    "not simulate a new trip and keep charging when SoC < 70 %" in {
      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        evAtChargingHub,
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
          uuid shouldBe ev2.getUuid
          id shouldBe "car_2"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe half
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN
          destinationPoi shouldBe charging_hub_townPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(4).plusMinutes(33)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe None
          finalDestinationPoi shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "not simulate a new trip and keep charging when SoC < 10 % and charging is available" in {
      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        evLowSoC,
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
          uuid shouldBe ev1.getUuid
          id shouldBe "car_1"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe zero
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.SHOPPING
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.SUPERMARKET
          destinationPoi shouldBe supermarket
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(1).plusMinutes(1)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe None
          finalDestinationPoi shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "simulate the next trip without additional charging stops" in {
      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        evNextTrip,
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
          uuid shouldBe ev3.getUuid
          id shouldBe "car_3"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe Quantities.getQuantity(
            40,
            PowerSystemUnits.KILOWATTHOUR
          )
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.SHOPPING
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.OTHER_SHOP
          destinationPoi shouldBe other_shopPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(1).plusMinutes(1)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe None
          finalDestinationPoi shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    // testing makeTripToChargingHub
    "makeTripToChargingHub correctly" in {
      TripSimulation.makeTripToChargingHub(
        PoiTypeDictionary.CHARGING_HUB_TOWN,
        ev4,
        givenSimulationStart,
        poisWithSizes,
        0.5,
        0.2,
        Quantities.getQuantity(1000, METRE),
        plannedDestinationPoi,
        PoiTypeDictionary.WORK,
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
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN
          destinationPoi shouldBe plannedDestinationPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(17)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe Some(PoiTypeDictionary.WORK)
          finalDestinationPoi shouldBe Some(plannedDestinationPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(3000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    // testing makeModifiedTripToChargingHub
    "makeModifiedTripToChargingHub correctly" in {
      TripSimulation.makeModifiedTripToChargingHub(
        PoiTypeDictionary.CHARGING_HUB_TOWN,
        ev4,
        givenSimulationStart,
        poisWithSizes,
        0.2,
        Quantities.getQuantity(1000, METRE),
        plannedDestinationPoi,
        PoiTypeDictionary.WORK,
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
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          destinationCategoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN
          destinationPoi shouldBe plannedDestinationPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(17)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe Some(PoiTypeDictionary.WORK)
          finalDestinationPoi shouldBe Some(plannedDestinationPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(10000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }
  }
}
