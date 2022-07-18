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
import java.time.ZonedDateTime
import java.util.UUID
import scala.util.Random

trait ChargingBehaviorTestData extends TripSimulationTestData {

  protected def evLowSoC: ElectricVehicle = ev1.copyWith(
    zero,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    destinationCategoricalLocation = CategoricalLocationDictionary.SUPERMARKET,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected def evAtChargingHub: ElectricVehicle = ev2.copyWith(
    half,
    destinationPoiType = PoiTypeDictionary.CHARGING_HUB_TOWN,
    destinationCategoricalLocation =
      CategoricalLocationDictionary.CHARGING_HUB_TOWN,
    destinationPoi = charging_hub_townPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected def evNextTrip: ElectricVehicle = ev3.copyWith(
    ev3.getEStorage,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    destinationCategoricalLocation = CategoricalLocationDictionary.OTHER_SHOP,
    destinationPoi = other_shopPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(1)
  )

  protected def evChargingNeeded: ElectricVehicle = ev4.copyWith(
    zero,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    destinationCategoricalLocation = CategoricalLocationDictionary.SUPERMARKET,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected def evNoChargingStations: ElectricVehicle = ev5.copyWith(
    zero,
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    destinationCategoricalLocation = CategoricalLocationDictionary.SUPERMARKET,
    destinationPoi = supermarketPoi,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val currentPricesAtChargingStations: Map[UUID, Double] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> 0.0
    }.toMap
  }

  protected val currentlyAvailableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> chargingStations.getChargingPoints
    }.toMap
  }

  protected val noAvailableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> 0
    }.toMap
  }

  protected def random: Random = new scala.util.Random(6)
}
