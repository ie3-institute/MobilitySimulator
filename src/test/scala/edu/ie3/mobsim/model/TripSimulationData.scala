/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.{PoiTestData, PoiUtils, PointOfInterest}
import edu.ie3.mobsim.io.probabilities.DrivingSpeed
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.KILOMETRE_PER_HOUR

import java.time.ZonedDateTime
import javax.measure.quantity.Energy

trait TripSimulationData extends ElectricVehicleTestData with PoiTestData {
  val ev: ElectricVehicle = ElectricVehicle.buildEv(
    "test_car",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    true
  )

  protected val chargingStations: Set[ChargingStation] =
    Set(cs0, cs1, cs2, cs3, cs4, cs5, cs6)

  protected val plannedDestinationPoi: PointOfInterest = itData(11)

  protected val pois: Set[PointOfInterest] = itData.toSet

  protected val poisWithSizes = PoiUtils.createPoiPdf(
    pois.map { poi =>
      poi.categoricalLocation -> Set(poi)
    }.toMap
  )

  private val speedFunction =
    SpeedFunction(50, 0, Quantities.getQuantity(50, KILOMETRE_PER_HOUR))

  private val speedMap = Range(0, 12).map(_ -> speedFunction).toMap

  protected val speed = DrivingSpeed(speedMap, speedMap, speedMap)

  protected val storedEnergyValue : ComparableQuantity[Energy] = Quantities.getQuantity(20, PowerSystemUnits.KILOWATTHOUR)
}
