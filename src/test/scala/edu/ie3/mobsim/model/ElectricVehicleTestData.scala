/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.datamodel.models.input.system.`type`.EvTypeInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.datamodel.models.input.system.characteristic.ReactivePowerCharacteristic
import edu.ie3.datamodel.models.voltagelevels.GermanVoltageLevelUtils
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction,
}
import edu.ie3.mobsim.utils.sq.KilowattHoursPerKilometer
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.*
import org.locationtech.jts.geom.Coordinate
import squants.Length
import squants.energy.{KilowattHours, Kilowatts}
import tech.units.indriya.quantity.Quantities
import squants.Dimensionless
import squants._

import java.time.ZonedDateTime
import java.util.UUID

trait ElectricVehicleTestData {
  protected val averageCarUsage = 1.0
  protected val givenModel: EvType = EvType(
    "cool_model",
    "cool_producer",
    "Van",
    KilowattHours(100d),
    KilowattHoursPerKilometer(10d),
    Kilowatts(11d),
    Kilowatts(50d),
    Each(1.0),
  )
  protected val givenModelPdf: ProbabilityDensityFunction[EvType] =
    ProbabilityDensityFunction(Map(givenModel -> 1.0))

  protected val givenHomePoi: PointOfInterest = PointOfInterest(
    UUID.fromString("d40b4feb-fd57-42b5-9247-43eaad2dff4b"),
    "test",
    CategoricalLocationDictionary.HOME,
    new Coordinate(7.4116482, 51.4843281),
    1.0,
    Map.empty[ChargingStation, Length],
  )
  protected val givenHomePoiPdf: ProbabilityDensityFunction[PointOfInterest] =
    ProbabilityDensityFunction(Map(givenHomePoi -> 1.0))

  protected val givenWorkPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("c34a031e-8568-4b59-99e4-1ad113bef6ea"),
    "test",
    CategoricalLocationDictionary.WORK,
    new Coordinate(7.4116482, 51.4843281),
    1.0,
    Map.empty[ChargingStation, Length],
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
      ProbabilityDensityFunction(departureProbability),
      round15 = false,
      averageCarUsage = averageCarUsage,
    )

  protected val givenChargingStation: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs",
    new Coordinate(7.4116482, 51.4843281),
    new ChargingPointType(
      "cs_type",
      Quantities.getQuantity(11d, PowerSystemUnits.KILOWATT),
      ElectricCurrentType.AC,
    ),
    EvcsLocationType.HOME,
    1,
  )

  protected val evWithHomeCharging: ElectricVehicle = ElectricVehicle.buildEv(
    "test_car",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible = true,
  )
  protected val evWithoutHomeCharging: ElectricVehicle = {
    evWithHomeCharging.copy(chargingAtHomePossible = false)
  }

  val node = new NodeInput(
    UUID.randomUUID(),
    "test-node",
    1d.asPu,
    false,
    NodeInput.DEFAULT_GEO_POSITION,
    GermanVoltageLevelUtils.LV,
    1,
  )

  protected val evInputType = new EvTypeInput(
    UUID.randomUUID(),
    "test-type",
    1d.asEuro,
    1d.asEuroPerKiloWattHour,
    50d.asKiloWattHour,
    5d.asKiloWattHourPerKiloMetre,
    11d.asKiloVoltAmpere,
    0.9,
    11d.asKiloWatt,
  )

  protected val evInput = new EvInput(
    UUID.randomUUID(),
    "test-ev-input",
    node,
    ReactivePowerCharacteristic.parse("cosPhiFixed:{(0.0, 0.95)}"),
    null,
    evInputType,
  )
}
