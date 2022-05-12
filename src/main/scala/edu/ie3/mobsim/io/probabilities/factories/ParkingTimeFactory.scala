/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.ParkingTime.ParkingTimeKey
import edu.ie3.mobsim.io.probabilities.{
  ParkingTime,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.utils.DayType

import scala.util.{Failure, Success, Try}

object ParkingTimeFactory extends ProbabilityFactory[ParkingTime] {
  private val uuid = "uuid"
  private val quarterHourOfDay = "quarter_hour_of_day"
  private val dayType = "day_type"
  private val poiType = "poi_type"
  private val duration = "duration"
  private val probability = "probability"
  override protected val requiredFields: Seq[String] =
    Seq(uuid, quarterHourOfDay, dayType, poiType, duration, probability)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[ParkingTime] = {
    val entryTrial = entityFieldData.map(Entry(_))
    entryTrial.find(_.isFailure) match {
      case Some(firstFailure: Failure[Entry]) =>
        Failure(
          SourceException(
            "Unable to build parking time probabilities. First failure in stack trace.",
            firstFailure.exception
          )
        )
      case Some(_) =>
        Failure(SourceException("Checking for failed entry creation failed."))
      case None =>
        val dayTypeToEntry = entryTrial
          .map(_.asInstanceOf[Success[Entry]].value)
          .groupBy(_.dayType)

        val weekday = dayTypeToEntry
          .get(DayType.WEEKDAY)
          .map(entries2pdf)
        val saturday = dayTypeToEntry
          .get(DayType.SATURDAY)
          .map(entries2pdf)
        val sunday = dayTypeToEntry
          .get(DayType.SUNDAY)
          .map(entries2pdf)

        weekday.zip(saturday).zip(sunday) match {
          case Some(
                ((weekdayProbability, saturdayProbability), sundayProbability)
              ) =>
            Success(
              ParkingTime(
                weekdayProbability,
                saturdayProbability,
                sundayProbability
              )
            )
          case None =>
            Failure(
              SourceException("Unable to determine all day type probabilities.")
            )
        }
    }
  }

  private def entries2pdf(
      entries: Seq[Entry]
  ): Map[ParkingTimeKey, ProbabilityDensityFunction[Int]] =
    entries
      .groupBy { case Entry(quarterHourOfDay, _, poiType, _, _) =>
        ParkingTimeKey(quarterHourOfDay, poiType)
      }
      .map { case key -> entries =>
        key -> ProbabilityDensityFunction(
          entries.map(entry => entry.duration -> entry.probability).toMap
        )
      }

  private final case class Entry(
      quarterHourOfDay: Int,
      dayType: DayType.Value,
      poiType: PoiTypeDictionary.Value,
      duration: Int,
      probability: Double
  )

  private object Entry {
    def apply(entityFieldData: Map[String, String]): Try[Entry] = Try {
      val qrtHrValue = entityFieldData
        .getOrElse(
          quarterHourOfDay,
          throw SourceException(
            "Unable to get quarter hour information from entity field data"
          )
        )
        .toInt
      val dayTypeValue = DayType(
        entityFieldData.getOrElse(
          dayType,
          throw SourceException(
            "Unable to get day type information from entity field data"
          )
        )
      ) match {
        case Failure(exception) => throw exception
        case Success(value)     => value
      }
      val poiTypeValue = PoiTypeDictionary(
        entityFieldData.getOrElse(
          poiType,
          throw SourceException(
            "Unable to get poi type information from entity field data"
          )
        )
      )
      val durationValue = entityFieldData
        .getOrElse(
          duration,
          throw SourceException(
            "Unable to get duration information from entity field data"
          )
        )
        .toInt
      val probabilityValue = entityFieldData
        .getOrElse(
          probability,
          throw SourceException(
            "Unable to get probability information from entity field data"
          )
        )
        .toDouble

      new Entry(
        qrtHrValue,
        dayTypeValue,
        poiTypeValue,
        durationValue,
        probabilityValue
      )
    }
  }
}
