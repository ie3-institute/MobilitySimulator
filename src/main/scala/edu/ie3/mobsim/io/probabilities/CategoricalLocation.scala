/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.probabilities.CategoricalLocation.CategoricalLocationKey

import java.time.{DayOfWeek, ZonedDateTime}

final case class CategoricalLocation(
    probabilitiesWeekday: Map[
      CategoricalLocationKey,
      ProbabilityDensityFunction[Int]
    ],
    probabilitiesSaturday: Map[
      CategoricalLocationKey,
      ProbabilityDensityFunction[Int]
    ],
    probabilitiesSunday: Map[
      CategoricalLocationKey,
      ProbabilityDensityFunction[Int]
    ]
) {

  /** Sample a categorical location dependent on day type, day time and the POI.
    * Using data from csv.
    *
    * @param time
    *   current time
    * @param poiType
    *   POI type for which the categorical location shall be sampled
    * @return
    *   sampled categorical location
    */
  def sample(
      time: ZonedDateTime,
      poiType: PoiTypeDictionary.Value
  ): CategoricalLocationDictionary.Value =
    poiType match {
      case PoiTypeDictionary.HOME =>
        CategoricalLocationDictionary.HOME // if home -> home
      case PoiTypeDictionary.WORK =>
        CategoricalLocationDictionary.WORK // if work -> work
      case _ =>
        /* Get current time on 15min basis */
        val timeInterval = time.getHour match {
          case 5 | 6 | 7 | 8 | 9      => 0
          case 10 | 11                => 1
          case 12 | 13                => 2
          case 14 | 15 | 16 | 17      => 3
          case 18 | 19 | 20 | 21 | 22 => 4
          case 23 | 0 | 1 | 2 | 3 | 4 => 5
          case _ => throw new RuntimeException("Time interval not found")
        }

        /* Get probabilities for the correct day */
        val probabilities
            : Map[CategoricalLocationKey, ProbabilityDensityFunction[Int]] =
          time.getDayOfWeek match {
            case DayOfWeek.SATURDAY => probabilitiesSaturday
            case DayOfWeek.SUNDAY   => probabilitiesSunday
            case _                  => probabilitiesWeekday
          }

        /* Sample categorical location */
        probabilities.get(
          CategoricalLocationKey(
            timeInterval,
            CategoricalLocationDictionary(poiType.toString)
          )
        ) match {
          case Some(pdf) => CategoricalLocationDictionary(pdf.sample())
          case _         => throw new RuntimeException("No pdf found")
        }
    }
}

case object CategoricalLocation {
  final case class CategoricalLocationKey(
      time: Int,
      poi: CategoricalLocationDictionary.Value
  )
}
