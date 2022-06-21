/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.model.EvTypeInput
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction
}
import edu.ie3.util.quantities.PowerSystemUnits
import org.locationtech.jts.geom.Coordinate
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.Length

trait ElectricVehicleTestData {
  protected val givenModel: EvTypeInput = EvTypeInput(
    "cool_model",
    "cool_producer",
    "Van",
    Quantities.getQuantity(100d, PowerSystemUnits.KILOWATTHOUR),
    Quantities
      .getQuantity(10d, PowerSystemUnits.KILOWATTHOUR_PER_KILOMETRE),
    Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
    Quantities.getQuantity(50d, PowerSystemUnits.KILOWATT)
  )
  protected val givenModelPdf: ProbabilityDensityFunction[EvTypeInput] =
    ProbabilityDensityFunction(Map(givenModel -> 1.0))

  protected val givenHomePoi: PointOfInterest = PointOfInterest(
    UUID.fromString("d40b4feb-fd57-42b5-9247-43eaad2dff4b"),
    "test",
    PoiTypeDictionary.HOME,
    CategoricalLocationDictionary.HOME,
    new Coordinate(7.4116482, 51.4843281),
    1.0,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )
  protected val givenHomePoiPdf: ProbabilityDensityFunction[PointOfInterest] =
    ProbabilityDensityFunction(Map(givenHomePoi -> 1.0))

  protected val givenWorkPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("c34a031e-8568-4b59-99e4-1ad113bef6ea"),
    "test",
    PoiTypeDictionary.WORK,
    CategoricalLocationDictionary.WORK,
    new Coordinate(7.4116482, 51.4843281),
    1.0,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )
  protected val givenWorkPoiPdf: ProbabilityDensityFunction[PointOfInterest] =
    ProbabilityDensityFunction(Map(givenWorkPoi -> 1.0))

  protected val givenSimulationStart: ZonedDateTime = ZonedDateTime.now()
  protected val givenFirstDeparture: ZonedDateTime =
    givenSimulationStart.plusHours(1L)

  private val departureProbability: Map[Int, Double] =
    Range.inclusive(0, 96).map(_ -> 1.0).toMap
  protected val givenFirstDepartureMetaData: FirstDepartureOfDay =
    FirstDepartureOfDay(
      ProbabilityDensityFunction(departureProbability),
      ProbabilityDensityFunction(departureProbability),
      ProbabilityDensityFunction(departureProbability)
    )

  protected val givenChargingStation: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs",
    new Coordinate(7.4116482, 51.4843281),
    new ChargingPointType(
      "cs_type",
      Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
      ElectricCurrentType.AC
    ),
    EvcsLocationType.HOME,
    1,
    homeChargingStationAssignedToPOI = true
  )
}
