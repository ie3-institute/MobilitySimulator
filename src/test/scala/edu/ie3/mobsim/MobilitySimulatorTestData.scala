/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import akka.actor.ActorRef
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary.{
  CHARGING_HUB_HIGHWAY,
  WORK
}
import edu.ie3.mobsim.model.{ChargingBehaviorTestData, ElectricVehicle}
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.ontology.builder.EvMovementsMessageBuilder

import java.util.UUID
import scala.collection.immutable.SortedSet

trait MobilitySimulatorTestData extends ChargingBehaviorTestData {
  val evData: ExtEvData = new ExtEvData(ActorRef.noSender, ActorRef.noSender)
  val builder = new EvMovementsMessageBuilder

  def setEvsAsParking(
      evs: SortedSet[ElectricVehicle]
  ): SortedSet[ElectricVehicle] = {
    evs.foreach { ev =>
      ev.copyWith(
        storedEnergy = zero,
        destinationPoiType = CHARGING_HUB_HIGHWAY,
        destinationCategoricalLocation =
          charging_hub_highwayPoi.categoricalLocation,
        destinationPoi = charging_hub_highwayPoi,
        parkingTimeStart = givenSimulationStart,
        departureTime = givenSimulationStart.plusHours(5)
      )
    }
    evs
  }

  def setEvsAsDeparting(
      evs: SortedSet[ElectricVehicle]
  ): SortedSet[ElectricVehicle] = {
    evs.foreach { ev =>
      ev.copyWith(
        storedEnergy = half,
        destinationPoiType = WORK,
        destinationCategoricalLocation = workPoi.categoricalLocation,
        destinationPoi = workPoi,
        parkingTimeStart = givenSimulationStart.plusHours(-4),
        departureTime = givenSimulationStart
      )

      ev.setChargingAtSimona(true)
      ev.setChosenChargingStation(Some(cs6.uuid))
    }
    evs
  }

  protected val chargingPointsAllTaken: Map[UUID, Int] = {
    Map(cs6.uuid -> 0)
  }

  protected val chargingPointsAllFree: Map[UUID, Int] = {
    Map(cs6.uuid -> cs6.chargingPoints)
  }

  protected val pricesAtChargingStation: Map[UUID, Double] = {
    Map(cs6.uuid -> 0.0)
  }

  protected val arrivingEv: ElectricVehicle = {
    ev1.copy(
      destinationPoi = charging_hub_highwayPoi,
      destinationPoiType = CHARGING_HUB_HIGHWAY,
      destinationCategoricalLocation =
        charging_hub_highwayPoi.categoricalLocation
    )
  }

  protected val evChargingAtSimonaWithStation: ElectricVehicle = {
    ev1.copy(
      chargingAtSimona = true,
      chosenChargingStation = Some(cs6.uuid)
    )
  }

  protected val evChargingAtSimonaWithoutStation: ElectricVehicle = {
    ev1.copy(chargingAtSimona = true)
  }

  val mobSim: MobilitySimulator = new MobilitySimulator(
    evData = evData,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = SortedSet(ev1, ev2, ev3, ev4, ev5),
    chargingHubTownIsPresent = true,
    chargingHubHighwayIsPresent = true,
    ioUtils = ioUtils,
    categoricalLocation = categoricalLocation,
    drivingSpeed = drivingSpeed,
    firstDepartureOfDay = firstDepartureOfDay,
    lastTripOfDay = lastTripOfDay,
    parkingTime = parkingTime,
    poiTransition = poiTransition,
    tripDistance = tripDistance,
    maxDistanceFromPoi = maxDistance,
    thresholdChargingHubDistance = maxDistance
  )

}
