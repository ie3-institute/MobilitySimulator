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
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Energy
import scala.util.Random

trait ChargingBehaviorTestData extends TripSimulationData {
  protected val zero: ComparableQuantity[Energy] =
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR)
  protected val half: ComparableQuantity[Energy] =
    Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR)

  val evChargingNeeded: ElectricVehicle = ev.copyWith(
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR),
    destinationPoiType = PoiTypeDictionary.SHOPPING,
    destinationCategoricalLocation =
      CategoricalLocationDictionary.SUPERMARKET,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val currentPricesAtChargingStations: Map[UUID, java.lang.Double] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> java.lang.Double.valueOf(0.0)
    }.toMap
  }

  protected val currentlyAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> Integer.valueOf(
        chargingStations.getChargingPoints
      )
    }.toMap
  }

  protected val noAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> Integer.valueOf(0)
    }.toMap
  }

  protected val seed: Random = new scala.util.Random(6)
}
