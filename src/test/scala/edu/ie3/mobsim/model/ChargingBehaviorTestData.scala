/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.Random

trait ChargingBehaviorTestData extends TripSimulationTestData {

  protected val evLowSoC: ElectricVehicle = ev1.copyWith(
    zero,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val evAtChargingHub: ElectricVehicle = ev2.copyWith(
    half,
    destinationPoi = charging_hub_townPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected val evNextTrip: ElectricVehicle = ev3.copyWith(
    ev3.getEStorage,
    destinationPoi = other_shopPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected val evChargingNeeded: ElectricVehicle = ev4.copyWith(
    zero,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val evNoChargingStations: ElectricVehicle = ev5.copyWith(
    zero,
    destinationPoi = supermarketPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val currentPricesAtChargingStations: Map[UUID, java.lang.Double] = {
    chargingStations.map { chargingStations =>
      chargingStations.uuid -> java.lang.Double.valueOf(0.0)
    }.toMap
  }

  protected val currentlyAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.uuid -> Integer.valueOf(
        chargingStations.chargingPoints
      )
    }.toMap
  }

  protected val noAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.uuid -> Integer.valueOf(0)
    }.toMap
  }

  protected def random: Random = new scala.util.Random(6)
  protected val availableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { chargingStations =>
      chargingStations.uuid -> chargingStations.chargingPoints
    }.toMap
  }
}
