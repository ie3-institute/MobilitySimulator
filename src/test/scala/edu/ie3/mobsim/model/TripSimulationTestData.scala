/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary,
}
import edu.ie3.mobsim.io.geodata.{
  PoiEnums,
  PoiTestData,
  PoiUtils,
  PointOfInterest,
}
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import edu.ie3.mobsim.io.probabilities._
import edu.ie3.mobsim.io.probabilities.factories._
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.KilowattHours
import squants.motion.KilometersPerHour
import squants.{Length, Meters}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.io.File
import java.nio.file.Paths
import java.time.ZonedDateTime
import javax.measure.quantity.Energy
import scala.util.{Failure, Success}

trait TripSimulationTestData extends ElectricVehicleTestData with PoiTestData {
  private val isChargingAtHomePossible: Boolean = true

  protected val zero: ComparableQuantity[Energy] =
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR)
  protected val half: ComparableQuantity[Energy] =
    Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR)

  val ev1: ElectricVehicle = ElectricVehicle.buildEv(
    "car_1",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible,
  )

  val ev2: ElectricVehicle = ElectricVehicle.buildEv(
    "car_2",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible,
  )

  val ev3: ElectricVehicle = ElectricVehicle.buildEv(
    "car_3",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible,
  )

  val ev4: ElectricVehicle = ElectricVehicle.buildEv(
    "car_4",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible,
  )

  val ev5: ElectricVehicle = ElectricVehicle.buildEv(
    "car_5",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible,
  )

  protected val chargingStations: Seq[ChargingStation] =
    Seq(cs0, cs1, cs2, cs3, cs4, cs5, cs6, cs7)

  private val poiData: Seq[PointOfInterest] = Seq(
    poiHome,
    workPoi,
    bbpgPoi,
    culturePoi,
    medicinalPoi,
    otherShopPoi,
    religiousPoi,
    restaurantPoi,
    servicePoi,
    sportsPoi,
    supermarketPoi,
    chargingHubTownPoi,
    chargingHubHighwayPoi,
  )

  protected val plannedDestinationPoi: PointOfInterest = poiData(11)

  protected val plannedStoredEnergyEndOfTrip: squants.Energy = KilowattHours(
    half.to(PowerSystemUnits.KILOWATTHOUR).getValue.doubleValue()
  )
  protected val plannedDestinationPoiType: PoiTypeDictionary.Value =
    PoiTypeDictionary.LEISURE
  protected val plannedDestinationCategoricalLocation
      : CategoricalLocationDictionary.Value =
    CategoricalLocationDictionary.CULTURE
  protected val plannedParkingTimeStart: ZonedDateTime =
    givenSimulationStart.plusMinutes(30)
  protected val plannedDepartureTime: ZonedDateTime =
    givenSimulationStart.plusHours(2)

  protected val pois: Set[PointOfInterest] = poiData.toSet

  protected val poisWithSizes: Map[
    PoiEnums.CategoricalLocationDictionary.Value,
    ProbabilityDensityFunction[PointOfInterest],
  ] = PoiUtils.createPoiPdf(
    pois.map { poi =>
      poi.categoricalLocation -> Set(poi)
    }.toMap
  )

  private val speedFunction: SpeedFunction =
    SpeedFunction(50, 0, KilometersPerHour(50))

  private val speedMap: Map[Int, SpeedFunction] =
    Range(0, 12).map(_ -> speedFunction).toMap

  protected val speed: DrivingSpeed = DrivingSpeed(speedMap, speedMap, speedMap)

  protected val storedEnergyValue: ComparableQuantity[Energy] =
    Quantities.getQuantity(20, PowerSystemUnits.KILOWATTHOUR)

  protected val maxDistance: Length =
    Meters(5000)

  private val basePath: String = Seq(
    Paths.get("").toAbsolutePath.toString,
    "src",
    "test",
    "resources",
    "edu",
    "ie3",
    "mobsim",
    "model",
  ).mkString(File.separator) + File.separator
  private val catLocFile: String = basePath + "categorical_location.csv"
  private val drivingSpeedFile: String = basePath + "driving_speed.csv"
  private val departureFile: String = basePath + "departure.csv"
  private val lastTripFile: String = basePath + "last_trip.csv"
  private val parkingTimeFile: String = basePath + "parking_time.csv"
  private val poiTransitionFile: String = basePath + "transition.csv"
  private val tripDistanceFile: String = basePath + "trip_distance.csv"

  protected val categoricalLocation: CategoricalLocation = {
    CategoricalLocationFactory.getFromFile(
      catLocFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val drivingSpeed: DrivingSpeed = {
    DrivingSpeedFactory.getFromFile(
      drivingSpeedFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val firstDepartureOfDay: FirstDepartureOfDay = {
    FirstDepartureFactory(averageCarUsage).getFromFile(
      departureFile,
      ",",
    ) match {
      case Success(value: FirstDepartureOfDay) => value
      case Failure(exception)                  => throw exception
    }
  }

  protected val lastTripOfDay: LastTripOfDay = {
    LastTripFactory.getFromFile(
      lastTripFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val parkingTime: ParkingTime = {
    ParkingTimeFactory.getFromFile(
      parkingTimeFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val poiTransition: PoiTransition = {
    PoiTransitionFactory.getFromFile(
      poiTransitionFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val tripDistance: TripDistance = {
    TripDistanceFactory.getFromFile(
      tripDistanceFile,
      ",",
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val tripProbabilities: TripProbabilities = TripProbabilities(
    categoricalLocation,
    drivingSpeed,
    firstDepartureOfDay,
    lastTripOfDay,
    parkingTime,
    poiTransition,
    tripDistance,
  )
}
