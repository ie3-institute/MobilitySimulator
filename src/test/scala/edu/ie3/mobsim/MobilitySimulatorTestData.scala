/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.mobsim.utils.IoUtilsTestData
import edu.ie3.simona.api.data.ev.ExtEvDataConnection

import java.util.UUID

trait MobilitySimulatorTestData extends IoUtilsTestData {
  val evDataConnection: ExtEvDataConnection = new ExtEvDataConnection()

  def setEvsAsParking(
      evs: Seq[ElectricVehicle]
  ): Seq[ElectricVehicle] =
    evs.map { ev =>
      ev.copy(
        storedEnergy = zero,
        destinationPoi = chargingHubHighwayPoi,
        destinationPoiType = PoiTypeDictionary.CHARGING_HUB_HIGHWAY,
        parkingTimeStart = givenSimulationStart,
        departureTime = givenSimulationStart.plusHours(5),
      )
    }

  def setEvsAsDeparting(
      evs: Seq[ElectricVehicle]
  ): Seq[ElectricVehicle] =
    evs.map { ev =>
      ev.copy(
        storedEnergy = half,
        destinationPoi = workPoi,
        destinationPoiType = PoiTypeDictionary.WORK,
        parkingTimeStart = givenSimulationStart.plusHours(-4),
        departureTime = givenSimulationStart,
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
      destinationPoi = chargingHubHighwayPoi,
      destinationPoiType = PoiTypeDictionary.CHARGING_HUB_HIGHWAY,
      parkingTimeStart = givenSimulationStart.plusMinutes(30),
    )
  }

  protected val departingEv: ElectricVehicle = {
    ev1.copy(
      destinationPoi = culturePoi,
      destinationPoiType = PoiTypeDictionary.LEISURE,
    )
  }

  protected val evChargingAtSimonaWithStation: ElectricVehicle = {
    ev1.copy(
      chargingAtSimona = true,
      chosenChargingStation = Some(cs6.uuid),
    )
  }

  protected val evChargingAtSimonaWithoutStation: ElectricVehicle = {
    ev1.copy(chargingAtSimona = true)
  }

  def mobSim(): MobilitySimulator = new MobilitySimulator(
    evDataConnection = evDataConnection,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = Seq(ev1, ev2, ev3, ev4, ev5),
    chargingHubTownIsPresent = true,
    chargingHubHighwayIsPresent = true,
    ioUtils = ioUtils,
    tripProbabilities,
    maxDistanceFromPoi = maxDistance,
    thresholdChargingHubDistance = maxDistance,
    round15 = false,
  )

}
