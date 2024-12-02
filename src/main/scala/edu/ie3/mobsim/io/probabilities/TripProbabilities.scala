/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.probabilities.factories.{
  CategoricalLocationFactory,
  DrivingSpeedFactory,
  FirstDepartureFactory,
  LastTripFactory,
  ParkingTimeFactory,
  PoiTransitionFactory,
  TripDistanceFactory,
}
import edu.ie3.mobsim.utils.PathsAndSources

import scala.util.{Failure, Success}

/** Container class to wrap probabilities for trip generation.
  *
  * @param categoricalLocation
  *   Needed meta information to determine next categorical location
  * @param drivingSpeed
  *   Needed meta information to determine next driving speed
  * @param firstDepartureOfDay
  *   Meta-information to determine the first departure of the day
  * @param lastTripOfDay
  *   Meta-information to determine if that trip is the last trip of the day
  * @param parkingTime
  *   Meta-information to determine the parking time
  * @param poiTransition
  *   Meta-information to determine the next POI transition
  * @param tripDistance
  *   Meta-information to determine the distance of the next trip
  */
final case class TripProbabilities(
    categoricalLocation: CategoricalLocation,
    drivingSpeed: DrivingSpeed,
    firstDepartureOfDay: FirstDepartureOfDay,
    lastTripOfDay: LastTripOfDay,
    parkingTime: ParkingTime,
    poiTransition: PoiTransition,
    tripDistance: TripDistance,
)

object TripProbabilities extends LazyLogging {

  def read(
      pathsAndSources: PathsAndSources,
      colSep: String,
      averageCarUsage: Double,
      round15: Boolean,
  ): TripProbabilities = {

    val firstDepartureOfDay =
      FirstDepartureFactory(averageCarUsage).getFromFile(
        pathsAndSources.firstDepartureOfDayPath,
        colSep,
      ) match {
        case Failure(exception) =>
          throw SourceException(
            "Unable to get probabilities for first departure of day from path.",
            exception,
          )
        case Success(value) => value.copy(round15 = round15)
      }

    val categoricalLocation = CategoricalLocationFactory.getFromFile(
      pathsAndSources.categoricalLocationPath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get categorical location probabilities from path.",
          exception,
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for categorical locations")

    val drivingSpeed = DrivingSpeedFactory.getFromFile(
      pathsAndSources.drivingSpeedPath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get driving speed parameters from path.",
          exception,
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for driving speed")

    val lastTripOfDay = LastTripFactory.getFromFile(
      pathsAndSources.lastTripPath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get last trip probabilities from path.",
          exception,
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for last trip of day")

    val parkingTime = ParkingTimeFactory.getFromFile(
      pathsAndSources.parkingTimePath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for parking time from path.",
          exception,
        )
      case Success(value: ParkingTime) => value.copy(round15 = round15)
    }
    logger.debug("Done loading probabilities for parking time")

    val poiTransition = PoiTransitionFactory.getFromFile(
      pathsAndSources.poiTransitionPath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for poi type transitions from path.",
          exception,
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for poi transition")

    val tripDistance = TripDistanceFactory.getFromFile(
      pathsAndSources.tripDistancePath,
      colSep,
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for trip distance transitions from path.",
          exception,
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for trip distance")

    TripProbabilities(
      categoricalLocation,
      drivingSpeed,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime,
      poiTransition,
      tripDistance,
    )
  }

}
