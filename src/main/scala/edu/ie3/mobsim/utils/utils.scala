/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit

object utils {

  /** Convert time to corresponding simulation tick
    * @param simulationStart
    *   Wall clock time of the simulation start
    * @param time
    *   time to be converted to tick
    * @return
    *   tick
    */
  def toTick(simulationStart: ZonedDateTime, time: ZonedDateTime): Long = {
    simulationStart.until(time, ChronoUnit.SECONDS)
  }

  /** Used to round driving time and parking time. We round to the closest
    * quarter hour. If the result is 0, we round to 15 minutes as driving and
    * parking time should never be 0.
    *
    * @param minutes
    *   minutes to be rounded
    *
    * @return
    *   rounded minutes (e.g. 15, 30, 45, 60, ..)
    */
  def roundToQuarterHourInMinutes(minutes: Int): Int = {
    if (minutes > 1440)
      throw new IllegalArgumentException(
        s"Value ($minutes) of Minutes exceed 1440 (= 1 Day). This should not happen."
      )

    val intervals15 = (minutes / 15) + math.round((minutes.toDouble % 15) / 15)
    intervals15 match {
      case 96 => 95 * 15
      case _  => math.max(15, intervals15 * 15).toInt
    }
  }
}
