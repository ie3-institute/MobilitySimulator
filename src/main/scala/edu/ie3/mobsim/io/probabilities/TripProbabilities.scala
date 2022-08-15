/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

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
case class TripProbabilities(
    categoricalLocation: CategoricalLocation,
    drivingSpeed: DrivingSpeed,
    firstDepartureOfDay: FirstDepartureOfDay,
    lastTripOfDay: LastTripOfDay,
    parkingTime: ParkingTime,
    poiTransition: PoiTransition,
    tripDistance: TripDistance
)
