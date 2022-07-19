/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import akka.actor.ActorRef
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary.WORK
import edu.ie3.mobsim.model.{ChargingBehaviorTestData, ElectricVehicle}
import edu.ie3.simona.api.data.ev.ExtEvData
import edu.ie3.simona.api.data.ev.ontology.builder.EvMovementsMessageBuilder

import java.util.UUID
import scala.collection.immutable.{SortedSet, TreeSet}

trait MobilitySimulatorTestData extends ChargingBehaviorTestData {
  val evData: ExtEvData = new ExtEvData(ActorRef.noSender, ActorRef.noSender)
  val builder = new EvMovementsMessageBuilder

  def electricVehicles(seq: Seq[ElectricVehicle]): SortedSet[ElectricVehicle] =
    SortedSet.empty[ElectricVehicle] ++ seq.toSet

  def evIsParking(evs: Seq[ElectricVehicle]): SortedSet[ElectricVehicle] = {
    evs.foreach { ev =>
      ev.copyWith(
        storedEnergy = half,
        destinationPoiType = WORK,
        destinationCategoricalLocation = workPoi.categoricalLocation,
        destinationPoi = workPoi,
        parkingTimeStart = givenSimulationStart,
        departureTime = givenSimulationStart.plusHours(5)
      )
    }

    TreeSet.empty[ElectricVehicle] ++ evs.toSet
  }

  def evIsDeparting(evs: Seq[ElectricVehicle]): SortedSet[ElectricVehicle] = {
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
      ev.setChosenChargingStation(Some(cs6.getUuid))
    }

    TreeSet.empty[ElectricVehicle] ++ evs.toSet
  }

  protected val chargingPointsAllTaken: Map[UUID, Integer] = {
    Map(cs6.getUuid -> Integer.valueOf(0))
  }

  protected val evChargingAtSimonaWithStation: ElectricVehicle = {
    ev1
    ev1.copy(chargingAtSimona = true)
    ev1.copy(chosenChargingStation = Some(cs6.getUuid))
  }

  protected val evChargingAtSimonaWithoutStation: ElectricVehicle = {
    ev1
    ev1.copy(chargingAtSimona = true)
  }

  val mobSim: MobilitySimulator = new MobilitySimulator(
    evData = evData,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = electricVehicles(Seq(ev1, ev2, ev3, ev4, ev5)),
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
