/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.TripDistance.TripDistanceKey
import edu.ie3.util.quantities.PowerSystemUnits.KILOMETRE
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

import java.time.{DayOfWeek, ZonedDateTime}
import javax.measure.quantity.Length

final case class TripDistance(
    probabilitiesWeekday: Map[TripDistanceKey, ProbabilityDensityFunction[
      Double
    ]],
    probabilitiesSaturday: Map[TripDistanceKey, ProbabilityDensityFunction[
      Double
    ]],
    probabilitiesSunday: Map[TripDistanceKey, ProbabilityDensityFunction[
      Double
    ]]
) {

  /** Sample a trip distance based on day type, day time and origin and
    * destination POI types. Using data from scv.
    * @param time
    *   current time
    * @param fromPoiType
    *   POI from where the trip starts
    * @param toPoiType
    *   POI where the trip ends
    * @return
    *   sampled trip distance
    */
  def sample(
      time: ZonedDateTime,
      fromPoiType: PoiTypeDictionary.Value,
      toPoiType: PoiTypeDictionary.Value
  ): ComparableQuantity[Length] = {

    /* Get current time on 15min basis */
    val timeQuarter = time.getHour * 4 + time.getMinute / 15

    /* Get probabilities for the correct day */
    val probabilities
        : Map[TripDistanceKey, ProbabilityDensityFunction[Double]] =
      time.getDayOfWeek match {
        case DayOfWeek.SATURDAY => probabilitiesSaturday
        case DayOfWeek.SUNDAY   => probabilitiesSunday
        case _                  => probabilitiesWeekday
      }

    /* Sample trip distance */
    val distance = probabilities.get(
      TripDistanceKey(
        timeQuarter,
        fromPoiType,
        toPoiType
      )
    ) match {
      case Some(pdf) => pdf.sample()
      case _         => throw new RuntimeException("No pdf found")
    }

    getQuantity(distance, KILOMETRE)
  }
}

case object TripDistance {
  final case class TripDistanceKey(
      time: Int,
      fromPoi: PoiTypeDictionary.Value,
      toPoi: PoiTypeDictionary.Value
  )
}
