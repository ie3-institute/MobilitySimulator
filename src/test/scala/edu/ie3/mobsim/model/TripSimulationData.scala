/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.{
  PoiEnums,
  PoiTestData,
  PoiUtils,
  PointOfInterest
}
import edu.ie3.mobsim.io.probabilities.{
  DrivingSpeed,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.KILOMETRE_PER_HOUR

import javax.measure.quantity.Energy

trait TripSimulationData extends ElectricVehicleTestData with PoiTestData {

  val ev: ElectricVehicle = ElectricVehicle.buildEv(
    "test_car",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible = true
  )

  protected val chargingStations: Set[ChargingStation] =
    Set(cs0, cs1, cs2, cs3, cs4, cs5, cs6)

  private val poiData: Seq[PointOfInterest] = Seq(
    poiHome,
    workPoi,
    bbpgPoi,
    culturePoi,
    medicinalPoi,
    other_shopPoi,
    religiousPoi,
    restaurantPoi,
    servicePoi,
    sportsPoi,
    supermarketPoi,
    charging_hub_townPoi,
    charging_hub_highwayPoi
  )

  protected val plannedDestinationPoi: PointOfInterest = poiData(11)

  protected val pois: Set[PointOfInterest] = poiData.toSet

  protected val poisWithSizes: Map[
    PoiEnums.CategoricalLocationDictionary.Value,
    ProbabilityDensityFunction[PointOfInterest]
  ] = PoiUtils.createPoiPdf(
    pois.map { poi =>
      poi.categoricalLocation -> Set(poi)
    }.toMap
  )

  private val speedFunction: SpeedFunction =
    SpeedFunction(50, 0, Quantities.getQuantity(50, KILOMETRE_PER_HOUR))

  private val speedMap: Map[Int, SpeedFunction] =
    Range(0, 12).map(_ -> speedFunction).toMap

  protected val speed: DrivingSpeed = DrivingSpeed(speedMap, speedMap, speedMap)

  protected val storedEnergyValue: ComparableQuantity[Energy] =
    Quantities.getQuantity(20, PowerSystemUnits.KILOWATTHOUR)
}
