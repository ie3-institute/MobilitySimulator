/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.MobilitySimulatorTestData
import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input.{Grid, Mobility}
import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{
  DrivingSpeed,
  FirstDepartureOfDay,
  LastTripOfDay,
  ParkingTime,
  TripProbabilities
}
import edu.ie3.mobsim.io.probabilities.factories.FirstDepartureFactory
import edu.ie3.mobsim.model.TripSimulation.{
  calculateDepartureTime,
  calculateStoredEnergyAtEndOfTrip
}
import edu.ie3.mobsim.model.TripSimulationItSpec.{
  firstDepartureOfDay,
  initializeEv
}
import edu.ie3.mobsim.utils.DefaultQuantities.ZERO_ENERGY
import edu.ie3.mobsim.utils.{IoUtils, PathsAndSources}
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOMETRE,
  KILOWATTHOUR,
  KILOWATTHOUR_PER_KILOMETRE
}
import org.locationtech.jts.geom.Coordinate
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{KILOMETRE_PER_HOUR, WATT}

import java.time.{LocalDateTime, ZonedDateTime}
import java.util.UUID
import javax.measure.quantity.{Energy, Length}
import scala.collection.immutable.Queue
import scala.util.{Failure, Success}

class TripSimulationItSpec extends UnitSpec with TripSimulationTestData {

//  currentTime: ZonedDateTime,
//  ev: ElectricVehicle,
//  poisWithSizes: Map[
//    CategoricalLocationDictionary.Value,
//    ProbabilityDensityFunction[PointOfInterest]
//  ],
//  chargingHubTownIsPresent: Boolean,
//  chargingHubHighwayIsPresent: Boolean,
//  chargingStations: Seq[ChargingStation],
//  ioUtils: IoUtils,
//  tripProbabilities: TripProbabilities,
//  thresholdChargingHubDistance: ComparableQuantity[Length]

  "Simulate the trips correctly" in {

    val mobilityConfig = Mobility(
      CsvParams(
        ",",
        "/Users/thomas/IdeaProjects/MobilitySimulator/input/mobilitySimulatorMid"
      )
    )
    val gridConfig = Grid(
      "test",
      CsvParams(
        ",",
        "/Users/thomas/IdeaProjects/simona/input/samples/novagent/grid"
      )
    )
    val inputConfig = Input(gridConfig, mobilityConfig)

    val pathsAndSources = PathsAndSources(
      "testSimulation",
      inputConfig,
      Some(
        "/Users/thomas/IdeaProjects/MobilitySimulator/input/mobilitySimulatorMid/output"
      )
    )

    val ioUtils = IoUtils(
      pathsAndSources.outputDir,
      "movements.csv",
      "evs.csv",
      "evcs.csv",
      "positions.csv",
      "pois.csv"
    )

    val firstDepartureOfDay = FirstDepartureFactory.getFromFile(
      pathsAndSources.firstDepartureOfDayPath,
      ","
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for first departure of day from path.",
          exception
        )
      case Success(value) => value
    }
    val simulationStart =
      TimeUtil.withDefaults.toZonedDateTime("2016-01-01 00:00:00")
    val firstDepartureEv = ???

//    val tripProbabilities = TripProbabilities(???)

//    val evs = (0to  100).map(_ => initializeEv(simulationStart, tripProbabilities))

//    TripSimulation.simulateNextTrip(???)
  }

}
object TripSimulationItSpec extends TripSimulationTestData {

  val evType: EvType = EvType(
    "myModel",
    "myProducer",
    "mySegment",
    Quantities.getQuantity(50, KILOWATTHOUR),
    Quantities.getQuantity(1, KILOWATTHOUR_PER_KILOMETRE),
    Quantities.getQuantity(1, WATT),
    Quantities.getQuantity(2, WATT)
  )

  def initializeEv(
      simulationStart: ZonedDateTime,
      tripProbabilities: TripProbabilities
  ): ElectricVehicle = {

    val firstDepartureTime =
      tripProbabilities.firstDepartureOfDay.sample(simulationStart.minusDays(1))
    val destinationLocation = tripProbabilities.poiTransition.sample(
      firstDepartureTime,
      PoiTypeDictionary.HOME
    )

    val plannedDrivingDistance = tripProbabilities.tripDistance.sample(
      firstDepartureTime,
      PoiTypeDictionary.HOME,
      destinationLocation
    )

    val (
      plannedStoredEnergyEndOfTrip,
      plannedParkingTimeStart,
      plannedDepartureTime
    ) = simulatePlannedTrip(
      firstDepartureTime,
      plannedDrivingDistance,
      destinationLocation,
      tripProbabilities.drivingSpeed,
      tripProbabilities.firstDepartureOfDay,
      tripProbabilities.lastTripOfDay,
      tripProbabilities.parkingTime
    )

    val departureTime = calculateDepartureTime(
      destinationLocation,
      plannedParkingTimeStart,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime
    )
    val destinationPoi = PointOfInterest(
      UUID.randomUUID(),
      destinationLocation.toString,
      CategoricalLocationDictionary.HOME,
      new Coordinate(1d, 1d),
      10,
      Map.empty
    )

    ElectricVehicle(
      simulationStart = simulationStart,
      uuid = UUID.randomUUID(),
      id = "myCar",
      evType = evType,
      homePoi = poiHome,
      workPoi = poiHome,
      storedEnergy = plannedStoredEnergyEndOfTrip,
      destinationPoi = destinationPoi,
      parkingTimeStart = plannedParkingTimeStart,
      departureTime = departureTime,
      chargingAtHomePossible = true,
      chosenChargingStation = None,
      chargingAtSimona = false,
      finalDestinationPoi = None,
      remainingDistanceAfterChargingHub = None,
      chargingPricesMemory = Queue.empty
    )

  }

  def simulatePlannedTrip(
      currentTime: ZonedDateTime,
      plannedDrivingDistance: ComparableQuantity[Length],
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      drivingSpeed: DrivingSpeed,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime
  ): (ComparableQuantity[Energy], ZonedDateTime, ZonedDateTime) = {

    /* Calculate stored energy at the end of the trip based on planned values */
    val plannedStoredEnergyEndOfTrip: ComparableQuantity[Energy] =
      calculateStoredEnergyAtEndOfTrip(
        evType.consumption,
        evType.capacity,
        plannedDrivingDistance: ComparableQuantity[Length]
      )

    /* Sample driving speed based on planned values */
    val plannedDrivingSpeed = drivingSpeed
      .sample(currentTime, plannedDrivingDistance)
      .to(KILOMETRE_PER_HOUR)

    /* Calculate driving time based on planned values */
    val plannedDrivingTime: Int = math.max(
      (math rint (plannedDrivingDistance
        .to(KILOMETRE)
        .divide(plannedDrivingSpeed.to(KILOMETRE_PER_HOUR))
        .getValue
        .doubleValue() * 60)).toInt,
      1
    )

    /* Calculate start of parking time based on planned values */
    val plannedParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(plannedDrivingTime)

    /* Calculate departure time based on planned values */
    val plannedDepartureTime: ZonedDateTime = calculateDepartureTime(
      plannedDestinationPoiType,
      plannedParkingTimeStart,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime
    )

    (
      plannedStoredEnergyEndOfTrip,
      plannedParkingTimeStart,
      plannedDepartureTime
    )
  }
}
