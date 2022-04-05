/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.io.geodata.PoiEnums
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.PoiTransition.PoiTransitionKey

import java.time.{DayOfWeek, ZonedDateTime}

final case class PoiTransition(
    probabilitiesWeekday: Map[PoiTransitionKey, ProbabilityDensityFunction[
      Int
    ]],
    probabilitiesSaturday: Map[PoiTransitionKey, ProbabilityDensityFunction[
      Int
    ]],
    probabilitiesSunday: Map[PoiTransitionKey, ProbabilityDensityFunction[Int]]
) {

  /** Sample a destination POI type dependent on day type, day time and previous
    * POI. Using data from csv.
    *
    * @param time
    *   current time
    * @param fromPoiType
    *   previous POI
    * @return
    *   sampled destination POI
    */
  def sample(
      time: ZonedDateTime,
      fromPoiType: PoiEnums.PoiTypeDictionary.Value
  ): PoiEnums.PoiTypeDictionary.Value = {
    /* Get current time on 15min basis */
    val timeQuarter = time.getHour * 4 + time.getMinute / 15

    /* Get probabilities for the correct day */
    val probabilities: Map[PoiTransitionKey, ProbabilityDensityFunction[Int]] =
      time.getDayOfWeek match {
        case DayOfWeek.SATURDAY => probabilitiesSaturday
        case DayOfWeek.SUNDAY   => probabilitiesSunday
        case _                  => probabilitiesWeekday
      }

    /* Sample destination POI type */
    probabilities.get(
      PoiTransitionKey(timeQuarter, fromPoiType.id)
    ) match {
      case Some(pdf) => PoiTypeDictionary(pdf.sample())
      case _         => throw new RuntimeException("No pdf found")
    }
  }
}

case object PoiTransition {
  final case class PoiTransitionKey(time: Int, poi: Int)
}
