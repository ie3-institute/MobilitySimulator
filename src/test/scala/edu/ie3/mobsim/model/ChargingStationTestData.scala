/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary

import java.time.ZonedDateTime
import java.util.UUID
import scala.util.Random

trait ChargingStationTestData extends TripSimulationTestData {

  protected val evLowSoC: ElectricVehicle = ev1.copyWith(
    zero,
    destinationPoi = supermarket,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val evAtChargingHub: ElectricVehicle = ev2.copyWith(
    half,
    destinationPoi = chargingHubTownPoi,
    destinationPoiType = PoiTypeDictionary.CHARGING_HUB_TOWN,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected val evNextTrip: ElectricVehicle = ev3.copyWith(
    ev3.getEStorage,
    destinationPoi = otherShopPoi,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected val evChargingNeeded: ElectricVehicle = ev4.copyWith(
    zero,
    destinationPoi = supermarket,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val evNoChargingStations: ElectricVehicle = ev5.copyWith(
    zero,
    destinationPoi = supermarketPoi,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val currentPricesAtChargingStations: Map[UUID, Double] = {
    chargingStations
      .map { chargingStation =>
        chargingStation.uuid -> 2.0
      }
      .toMap
      .updated(cs7.uuid, 10.0)
  }

  protected val chargingStationOccupancy: Map[UUID, Seq[ElectricVehicle]] = {
    Seq(ev1, ev2, ev3, ev4, ev5)
      .flatMap { ev =>
        ev.chosenChargingStation.map(ev -> _)
      }
      .groupMap { case (_, cs) =>
        cs
      } { case (ev, _) =>
        ev
      }
  }

  protected val currentlyAvailableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { cs =>
      cs.uuid -> cs.chargingPoints
    }.toMap
  }

  protected val noAvailableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { _.uuid -> 0 }.toMap
  }

  protected def random: Random = new scala.util.Random(6)
  protected val availableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { chargingStations =>
      chargingStations.uuid -> chargingStations.chargingPoints
    }.toMap
  }
}
