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
import edu.ie3.mobsim.io.geodata.{
  PoiEnums,
  PoiTestData,
  PoiUtils,
  PointOfInterest
}
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import edu.ie3.mobsim.io.probabilities.factories.{
  CategoricalLocationFactory,
  FirstDepartureFactory,
  LastTripFactory,
  ParkingTimeFactory,
  PoiTransitionFactory,
  TripDistanceFactory
}
import edu.ie3.mobsim.io.probabilities.{
  CategoricalLocation,
  DrivingSpeed,
  FirstDepartureOfDay,
  LastTripOfDay,
  ParkingTime,
  PoiTransition,
  ProbabilityDensityFunction,
  TripDistance
}
import edu.ie3.mobsim.utils.IoUtils
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.{KILOMETRE_PER_HOUR, METRE}

import java.io.File
import java.time.ZonedDateTime
import javax.measure.quantity.{Energy, Length}
import scala.util.{Failure, Success}

trait TripSimulationData extends ElectricVehicleTestData with PoiTestData {
  private val isChargingAtHomePossible: Boolean = true

  protected val zero: ComparableQuantity[Energy] =
    Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR)
  protected val half: ComparableQuantity[Energy] =
    Quantities.getQuantity(50, PowerSystemUnits.KILOWATTHOUR)

  def ev1: ElectricVehicle = ElectricVehicle.buildEv(
    "car_1",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible
  )

  def ev2: ElectricVehicle = ElectricVehicle.buildEv(
    "car_2",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible
  )

  def ev3: ElectricVehicle = ElectricVehicle.buildEv(
    "car_3",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible
  )

  def ev4: ElectricVehicle = ElectricVehicle.buildEv(
    "car_4",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible
  )

  def ev5: ElectricVehicle = ElectricVehicle.buildEv(
    "car_5",
    givenModel,
    givenHomePoi,
    givenWorkPoi,
    givenSimulationStart,
    givenFirstDeparture,
    isChargingAtHomePossible
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

  protected val plannedStoredEnergyEndOfTrip: ComparableQuantity[Energy] = half
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
    ProbabilityDensityFunction[PointOfInterest]
  ] = PoiUtils.createPoiPdf(
    pois.map { poi =>
      poi.categoricalLocation -> Set(poi)
    }.toMap
  )

  private val nextPoi: Set[PointOfInterest] = Seq(other_shopPoi).toSet

  protected val nextDestinationPoi: Map[
    PoiEnums.CategoricalLocationDictionary.Value,
    ProbabilityDensityFunction[PointOfInterest]
  ] = PoiUtils.createPoiPdf(
    nextPoi.map { poi =>
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

  protected val maxDistance: ComparableQuantity[Length] =
    Quantities.getQuantity(5000, METRE)

  protected val ioUtils: IoUtils = IoUtils(
    new java.io.File(
      "."
    ).getCanonicalPath + File.separator + "out" + File.separator,
    "movements",
    "evs",
    "evcs",
    "positions",
    "pois"
  )

  protected val categoricalLocation: CategoricalLocation = {
    CategoricalLocationFactory.getFromFile(
      this.getClass.getResource("categorical_location.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val firstDepartureOfDay: FirstDepartureOfDay = {
    FirstDepartureFactory.getFromFile(
      this.getClass.getResource("departure.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val lastTripOfDay: LastTripOfDay = {
    LastTripFactory.getFromFile(
      this.getClass.getResource("last_trip.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val parkingTime: ParkingTime = {
    ParkingTimeFactory.getFromFile(
      this.getClass.getResource("parking_time.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val poiTransition: PoiTransition = {
    PoiTransitionFactory.getFromFile(
      this.getClass.getResource("transition.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }

  protected val tripDistance: TripDistance = {
    TripDistanceFactory.getFromFile(
      this.getClass.getResource("trip_distance.csv").getFile,
      ","
    ) match {
      case Success(value)     => value
      case Failure(exception) => throw exception
    }
  }
}
