/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import akka.actor.ActorRef
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.mobsim.utils.IoUtilsTestData
import edu.ie3.simona.api.data.ev.ExtEvData

import java.util.UUID
import scala.collection.immutable.SortedSet

trait MobilitySimulatorTestData extends IoUtilsTestData {
  val evData: ExtEvData = new ExtEvData(ActorRef.noSender, ActorRef.noSender)
  val builder = new EvMovementsMessageBuilder

  def setEvsAsParking(
      evs: SortedSet[ElectricVehicle]
  ): SortedSet[ElectricVehicle] =
    evs.map { ev =>
      ev.copy(
        storedEnergy = zero,
        destinationPoi = charging_hub_highwayPoi,
        destinationPoiType = PoiTypeDictionary.CHARGING_HUB_HIGHWAY,
        parkingTimeStart = givenSimulationStart,
        departureTime = givenSimulationStart.plusHours(5)
      )
    }

  def setEvsAsDeparting(
      evs: SortedSet[ElectricVehicle]
  ): SortedSet[ElectricVehicle] =
    evs.map { ev =>
      ev.copy(
        storedEnergy = half,
        destinationPoi = workPoi,
        destinationPoiType = PoiTypeDictionary.WORK,
        parkingTimeStart = givenSimulationStart.plusHours(-4),
        departureTime = givenSimulationStart
      ).setChargingAtSimona()
        .setChosenChargingStation(Some(cs6.uuid))
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
      destinationPoiType = PoiTypeDictionary.CHARGING_HUB_HIGHWAY
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

  def mobSim(): MobilitySimulator = new MobilitySimulator(
    evData = evData,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = SortedSet(ev1, ev2, ev3, ev4, ev5),
    chargingHubTownIsPresent = true,
    chargingHubHighwayIsPresent = true,
    ioUtils = ioUtils,
    tripProbabilities,
    maxDistanceFromPoi = maxDistance,
    thresholdChargingHubDistance = maxDistance
  )

}
