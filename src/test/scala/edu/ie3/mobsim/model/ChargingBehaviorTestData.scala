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
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Energy, Length}
import scala.util.Random

trait ChargingBehaviorTestData extends TripSimulationData {
  protected val zero: ComparableQuantity[Energy] =
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR)
  protected val half: ComparableQuantity[Energy] =
    Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR)

  val evChargingNeeded: ElectricVehicle = ev.copyWith(
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR),
    destinationPoiType = PoiTypeDictionary.SHOPPING.id,
    destinationCategoricalLocation =
      CategoricalLocationDictionary.SUPERMARKET.id,
    destinationPoi = supermarket,
    parkingTimeStart = ZonedDateTime.now(),
    departureTime = ZonedDateTime.now().plusHours(5)
  )

  protected val currentPricesAtChargingStations: Map[UUID, java.lang.Double] = {
    val price: java.lang.Double = 0.0
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> price
    }.toMap
  }

  protected val currentlyAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> chargingStations.getChargingPoints
        .asInstanceOf[Integer]
    }.toMap
  }

  protected val noAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> 0.asInstanceOf[Integer]
    }.toMap
  }

  protected val seed: Random = new scala.util.Random(6)

  protected val maxDistance: ComparableQuantity[Length] =
    Quantities.getQuantity(5000, Units.METRE)

}
