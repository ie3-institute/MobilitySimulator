/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.ParkingTime.ParkingTimeKey
import edu.ie3.mobsim.utils.utils.roundToQuarterHourInMinutes

import java.time.{DayOfWeek, ZonedDateTime}

final case class ParkingTime(
    probabilitiesWeekday: Map[ParkingTimeKey, ProbabilityDensityFunction[Int]],
    probabilitiesSaturday: Map[ParkingTimeKey, ProbabilityDensityFunction[Int]],
    probabilitiesSunday: Map[ParkingTimeKey, ProbabilityDensityFunction[Int]],
    round15: Boolean
) {

  /** Sample a parking time dependent on day type, day time and POI type. Using
    * data from csv.
    *
    * @param time
    *   arrival time
    * @param poiType
    *   POI where the EV is parking
    * @return
    *   sampled parking time
    */
  def sample(time: ZonedDateTime, poiType: PoiTypeDictionary.Value): Int = {

    /* Get current time on 15min basis */
    val timeQuarter = time.getHour * 4 + time.getMinute / 15

    /* Get probabilities for the correct day */
    val probabilities: Map[ParkingTimeKey, ProbabilityDensityFunction[Int]] =
      time.getDayOfWeek match {
        case DayOfWeek.SATURDAY => probabilitiesSaturday
        case DayOfWeek.SUNDAY   => probabilitiesSunday
        case _                  => probabilitiesWeekday
      }

    /* Sample parking time */
    val parkingTime = probabilities.get(
      ParkingTimeKey(timeQuarter, poiType)
    ) match {
      case Some(pdf) => {
        val sampledTime = pdf.sample()
        if (round15) {
          roundToQuarterHourInMinutes(sampledTime)
        } else sampledTime
      }
      case _ =>
        throw new RuntimeException(
          "No pdf found"
        ) // TODO: Work with checked exceptions
    }

    /* if parking time is 0 minutes, make at least 1 minute (necessary for sending / receiving from SIMONA) */
    if (parkingTime > 0) parkingTime else 1
  }
}

case object ParkingTime {
  final case class ParkingTimeKey(time: Int, poi: PoiTypeDictionary.Value)
}
