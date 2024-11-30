/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import java.time.{DayOfWeek, ZonedDateTime}
import scala.util.Random

final case class LastTripOfDay(
    probabilityWeekday: Map[Int, Double],
    probabilitySaturday: Map[Int, Double],
    probabilitySunday: Map[Int, Double],
) {

  /** Sample if the trip was last trip of day dependent on day type and day
    * time. Using data from csv.
    *
    * @param time
    *   arrival time
    * @return
    *   sampled Boolean, whether it is the last trip
    */
  def sample(time: ZonedDateTime, seed: Random): Boolean = {

    /* Get current time on 15min basis */
    val timeQuarter = time.getHour * 4 + time.getMinute / 15

    /* Get probabilities for the correct day */
    val probabilities: Map[Int, Double] =
      time.getDayOfWeek match {
        case DayOfWeek.SATURDAY => probabilitySaturday
        case DayOfWeek.SUNDAY   => probabilitySunday
        case _                  => probabilityWeekday
      }

    probabilities.get(timeQuarter) match {
      case Some(value) =>
        seed.nextDouble() <= value
      case None =>
        throw new RuntimeException("No probability for given time found")
    }
  }
}
