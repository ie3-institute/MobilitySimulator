/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.utils.utils

import edu.ie3.mobsim.io.probabilities.FirstDepartureOfDay.random

import java.time.{DayOfWeek, ZoneId, ZonedDateTime}
import scala.annotation.tailrec
import scala.util.Random

final case class FirstDepartureOfDay(
    probabilityWeekday: ProbabilityDensityFunction[Int],
    probabilitySaturday: ProbabilityDensityFunction[Int],
    probabilitySunday: ProbabilityDensityFunction[Int],
    averageCarUsage: Double,
    round15: Boolean,
) {

  /** Sample the first departure time on a day dependent on day type. Using data
    * from csv.
    *
    * @param time
    *   arrival time
    * @return
    *   Departure time as ZonedDateTime
    */
  def sample(time: ZonedDateTime): ZonedDateTime = {

    @tailrec
    def sampleNextDayWithNewTrip(time: ZonedDateTime): ZonedDateTime = {
      if (random.nextDouble() <= averageCarUsage) time.plusDays(1)
      else sampleNextDayWithNewTrip(time.plusDays(1))
    }

    /* We can say that the departure will be on the next day ( = today + 1 day) to get year, month and day.
     * Hour and minute can be determined using departureTimeAsInt. */
    val nextDayWithNewTrip = sampleNextDayWithNewTrip(time)

    /* Sample time of first departure as Int */
    val rawDepartureTimeAsInt: Int = nextDayWithNewTrip.getDayOfWeek match {
      case DayOfWeek.SATURDAY => probabilitySaturday.sample()
      case DayOfWeek.SUNDAY   => probabilitySunday.sample()
      case _                  => probabilityWeekday.sample()
    }

    val departureTimeAsInt = if (round15) {
      utils.roundToQuarterHourInMinutes(rawDepartureTimeAsInt)
    } else
      rawDepartureTimeAsInt

    val firstDeparture = ZonedDateTime.of(
      nextDayWithNewTrip.getYear,
      nextDayWithNewTrip.getMonthValue,
      nextDayWithNewTrip.getDayOfMonth,
      departureTimeAsInt / 60,
      departureTimeAsInt % 60,
      0,
      0,
      ZoneId.of("UTC")
    )

    /* Departure time cannot be same as current time, so in that case add one minute. */
    if (firstDeparture == time) firstDeparture.plusMinutes(1)
    else firstDeparture
  }
}

object FirstDepartureOfDay {
  val random: Random = new Random()
}
