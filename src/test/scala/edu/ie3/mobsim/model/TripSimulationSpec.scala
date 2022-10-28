/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.utils.IoUtilsTestData
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{METRE, SECOND}

import java.time.temporal.TemporalUnit
import java.time.{LocalDateTime, ZonedDateTime}
import javax.measure.quantity.Energy
import scala.collection.mutable

class TripSimulationSpec extends UnitSpec with IoUtilsTestData {

  "TripSimulation" should {
    "not simulate a new trip and keep charging when SoC < 70 and is at charging hub %" in {
      TripSimulation.simulateNextTrip(
        givenSimulationStart,
        evAtChargingHub,
        poisWithSizes,
        chargingHubTownIsPresent = true,
        chargingHubHighwayIsPresent = true,
        chargingStations,
        ioUtils,
        tripProbabilities,
        maxDistance,
        round15 = false
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoi,
              destinationPoiType,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev2.getUuid
          id shouldBe "car_2"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe half
          chargingAtSimona shouldBe false
          destinationPoi shouldBe charging_hub_townPoi
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(4).plusMinutes(33)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe None
          finalDestinationPoiType shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "round trip times to 15 min values correctly" in {
      TripSimulation.simulateNextTrip(
        givenSimulationStart.withMinute(0),
        ev4,
        poisWithSizes,
        chargingHubTownIsPresent = true,
        chargingHubHighwayIsPresent = true,
        chargingStations,
        ioUtils,
        tripProbabilities,
        maxDistance,
        round15 = true
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              _,
              _,
              _,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              _,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          parkingTimeStart.getMinute % 15 shouldBe 0
          departureTime.getMinute % 15 shouldBe 0
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe None
          finalDestinationPoiType shouldBe None
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
        tripProbabilities,
        maxDistance,
        round15 = false
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoi,
              destinationPoiType,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev1.getUuid
          id shouldBe "car_1"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe zero
          chargingAtSimona shouldBe false
          destinationPoi shouldBe supermarket
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(1).plusMinutes(1)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

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
        plannedDestinationPoiType,
        chargingStations,
        speed
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoi,
              destinationPoiType,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoi shouldBe charging_hub_townPoi
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          parkingTimeStart shouldBe simulationStart.plusMinutes(10)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(26)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe Some(plannedDestinationPoi)
          finalDestinationPoiType shouldBe Some(plannedDestinationPoiType)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(-7000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "makeModifiedTripToChargingHub correctly" in {
      TripSimulation.makeModifiedTripToChargingHub(
        PoiTypeDictionary.CHARGING_HUB_TOWN,
        ev4,
        givenSimulationStart,
        poisWithSizes,
        0.2,
        Quantities.getQuantity(1000, METRE),
        supermarketPoi,
        PoiTypeDictionary.SHOPPING,
        chargingStations,
        speed
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoi,
              destinationPoiType,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe storedEnergyValue
          chargingAtSimona shouldBe false
          destinationPoi shouldBe charging_hub_townPoi
          destinationPoiType shouldBe PoiTypeDictionary.CHARGING_HUB_TOWN
          parkingTimeStart shouldBe simulationStart.plusMinutes(1)
          departureTime shouldBe simulationStart.plusHours(7).plusMinutes(17)
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe Some(supermarketPoi)
          remainingDistanceAfterChargingHub shouldBe Some(
            Quantities.getQuantity(10000, METRE)
          )
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "keep the original trip" in {
      TripSimulation.keepOriginalTrip(
        ev4,
        plannedStoredEnergyEndOfTrip,
        plannedDestinationPoi,
        plannedDestinationPoiType,
        plannedParkingTimeStart,
        plannedDepartureTime
      ) match {
        case ElectricVehicle(
              simulationStart,
              uuid,
              id,
              evType,
              homePoi,
              workPoi,
              storedEnergy,
              destinationPoi,
              destinationPoiType,
              parkingTimeStart,
              departureTime,
              chargingAtHomePossible,
              chosenChargingStation,
              chargingAtSimona,
              finalDestinationPoi,
              finalDestinationPoiType,
              remainingDistanceAfterChargingHub,
              chargingPricesMemory
            ) =>
          simulationStart shouldBe givenSimulationStart
          uuid shouldBe ev4.getUuid
          id shouldBe "car_4"
          evType shouldBe givenModel
          homePoi shouldBe givenHomePoi
          workPoi shouldBe givenWorkPoi
          storedEnergy shouldBe plannedStoredEnergyEndOfTrip
          chargingAtSimona shouldBe false
          destinationPoi shouldBe plannedDestinationPoi
          destinationPoiType shouldBe plannedDestinationPoiType
          parkingTimeStart shouldBe plannedParkingTimeStart
          departureTime shouldBe plannedDepartureTime
          chargingAtHomePossible shouldBe true
          chosenChargingStation shouldBe None
          finalDestinationPoi shouldBe None
          finalDestinationPoiType shouldBe None
          remainingDistanceAfterChargingHub shouldBe None
          chargingPricesMemory shouldBe mutable.Queue[Double]()
      }
    }

    "calculate stored energy at the end of trip" in {
      val energy: ComparableQuantity[Energy] =
        TripSimulation.calculateStoredEnergyAtEndOfTrip(
          ev4.evType.consumption,
          ev4.getEStorage,
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
