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
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.METRE

import java.time.ZonedDateTime
import javax.measure.quantity.Energy
import scala.collection.mutable

class TripSimulationSpec extends UnitSpec with TripSimulationTestData {
class TripSimulationSpec extends UnitSpec with ChargingBehaviorTestData {

  "TripSimulation" should {
    "not simulate a new trip and keep charging when SoC < 70 %" in {
      val ev: ElectricVehicle = evAtChargingHub

      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        ev,
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
          uuid shouldBe ev.getUuid
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
      val ev: ElectricVehicle = evLowSoC

      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        ev,
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
          uuid shouldBe ev.getUuid
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

    // testing makeTripToChargingHub
    "makeTripToChargingHub correctly" in {
      val ev: ElectricVehicle = ev4

      val updatedEv: ElectricVehicle = TripSimulation.makeTripToChargingHub(
        PoiTypeDictionary.CHARGING_HUB_TOWN,
        ev1,
        givenSimulationStart,
        poisWithSizes,
        0.5,
        0.2,
        Quantities.getQuantity(1000, METRE),
        plannedDestinationPoi,
        chargingStations,
        speed
      )

      updatedEv match {
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
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev.getUuid
          id shouldBe"car_4"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoi.getPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          destinationPoi.categoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN
          destinationPoi shouldBe charging_hub_townPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(10)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(26)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi.map(_.getPoiType) shouldBe Some(
            supermarketPoi.getPoiType
          )
          finalDestinationPoi shouldBe Some(supermarketPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(-7000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    // testing makeModifiedTripToChargingHub
    "makeModifiedTripToChargingHub correctly" in {
      val ev: ElectricVehicle = ev4

      val updatedEv: ElectricVehicle =
        TripSimulation.makeModifiedTripToChargingHub(
          PoiTypeDictionary.CHARGING_HUB_TOWN,
          ev,
          givenSimulationStart,
          poisWithSizes,
          0.2,
          Quantities.getQuantity(1000, METRE),
          plannedDestinationPoi,
          chargingStations,
          speed
        )

      updatedEv match {
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
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev.getUuid
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
          destinationPoi.getPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          destinationPoi.categoricalLocation shouldBe CategoricalLocationDictionary.CHARGING_HUB_TOWN
          destinationPoi shouldBe charging_hub_townPoi
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(17)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi.map(_.getPoiType) shouldBe Some(
            supermarketPoi.getPoiType
          )
          finalDestinationPoi shouldBe Some(supermarketPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(10000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "keep the original trip" in {
      val ev: ElectricVehicle = ev4

      TripSimulation.keepOriginalTrip(
        ev,
        plannedStoredEnergyEndOfTrip,
        plannedDestinationPoiType,
        plannedDestinationCategoricalLocation,
        plannedDestinationPoi,
        plannedParkingTimeStart,
        plannedDepartureTime
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
              destinationPoi,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,

              finalDestinationPoi,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev.getUuid
          id shouldBe "car_4"
          model shouldBe "cool_producer cool_model"
          batteryCapacity shouldBe givenModel.capacity
          acChargingPower shouldBe givenModel.acPower
          dcChargingPower shouldBe givenModel.dcPower
          consumption shouldBe givenModel.consumption
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe plannedStoredEnergyEndOfTrip
          chargingAtSimona shouldBe false
          destinationPoiType shouldBe plannedDestinationPoiType
          destinationCategoricalLocation shouldBe plannedDestinationCategoricalLocation
          destinationPoi shouldBe plannedDestinationPoi
          parkingTimeStart shouldBe plannedParkingTimeStart
          departureTime shouldBe plannedDepartureTime
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoiType shouldBe None
          finalDestinationPoi shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "calculate stored energy at the end of trip" in {
      val energy: ComparableQuantity[Energy] =
        TripSimulation.calculateStoredEnergyAtEndOfTrip(
          ev4.copyWith(ev4.getEStorage),
          drivingDistance = Quantities.getQuantity(4000, METRE)
        )

      energy shouldBe Quantities.getQuantity(60d, PowerSystemUnits.KILOWATTHOUR)
    }

    "calculate departure time" in {
      val time: ZonedDateTime = TripSimulation.calculateDepartureTime(
        plannedDestinationPoiType,
        plannedParkingTimeStart,
        firstDepartureOfDay,
        lastTripOfDay,
        parkingTime
      )

      time shouldBe plannedParkingTimeStart.plusHours(1)
    }
  }
}
