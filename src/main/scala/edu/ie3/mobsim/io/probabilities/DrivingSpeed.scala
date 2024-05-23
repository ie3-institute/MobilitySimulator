/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import squants.Length
import squants.motion.{KilometersPerHour, Velocity}

import java.time.{DayOfWeek, ZonedDateTime}

/** Container class for function parameters to calculate the driving speed of a
  * car
  *
  * @param parametersWeekday
  *   Function parameters for weekdays
  * @param parametersSaturday
  *   Function parameters for saturdays
  * @param parametersSunday
  *   Function parameters for sundays
  */
final case class DrivingSpeed(
    parametersWeekday: Map[Int, SpeedFunction],
    parametersSaturday: Map[Int, SpeedFunction],
    parametersSunday: Map[Int, SpeedFunction]
) {

  /** Sample a driving speed dependent on day type, day time and the distance to
    * drive. Using data from csv.
    *
    * @param time
    *   current time
    * @param distance
    *   distance of the trip
    * @return
    *   sampled driving speed
    */
  def sample(
      time: ZonedDateTime,
      distance: Length
  ): Velocity = {

    /* Get current time on 15min basis */
    val timeInterval = time.getHour match {
      case 0 | 1   => 0
      case 2 | 3   => 1
      case 4 | 5   => 2
      case 6 | 7   => 3
      case 8 | 9   => 4
      case 10 | 11 => 5
      case 12 | 13 => 6
      case 14 | 15 => 7
      case 16 | 17 => 8
      case 18 | 19 => 9
      case 20 | 21 => 10
      case 22 | 23 => 11
    }

    /* Get parameters for the correct day */
    val parameters =
      time.getDayOfWeek match {
        case DayOfWeek.SATURDAY => parametersSaturday
        case DayOfWeek.SUNDAY   => parametersSunday
        case _                  => parametersWeekday
      }

    /* Sample driving speed */
    parameters
      .get(timeInterval)
      .map(_.calculate(distance))
      .getOrElse(
        throw new RuntimeException(
          "Unable to sample driving speed, as no function parameters can be found."
        )
      )
  }
}

case object DrivingSpeed extends LazyLogging {

  /** Function (a + b * log(distance)) to calculate the estimated driving speed
    * of the trip
    *
    * @param a
    *   Offset
    * @param b
    *   Scaling factor
    * @param minimumSpeed
    *   Minimum permissible speed
    */
  final case class SpeedFunction(
      a: Double,
      b: Double,
      minimumSpeed: Velocity
  ) {

    /** Calculate the driving speed according to the distance
      * @param distance
      *   Distance to drive
      * @return
      *   Average driving speed
      */
    def calculate(
        distance: Length
    ): Velocity = {
      val safeDistance =
        math.max(distance.toKilometers, 1.0)
      val proposedSpeed = KilometersPerHour(
        a + b * math.log(safeDistance)
      )
      if (proposedSpeed < minimumSpeed)
        minimumSpeed
      else
        proposedSpeed
    }
  }

  /** Conversion from time interval written as string from csv file to integer
    * @param string
    *   time interval from csv file
    * @return
    *   integer for time interval
    */
  def timeFromStringToInt(string: String): Int = {
    val int = string match {
      case "0:00-2:00"   => 0
      case "2:00-4:00"   => 1
      case "4:00-6:00"   => 2
      case "6:00-8:00"   => 3
      case "8:00-10:00"  => 4
      case "10:00-12:00" => 5
      case "12:00-14:00" => 6
      case "14:00-16:00" => 7
      case "16:00-18:00" => 8
      case "18:00-20:00" => 9
      case "20:00-22:00" => 10
      case "22:00-24:00" => 11
      case _             => 400
    }
    int
  }
}
