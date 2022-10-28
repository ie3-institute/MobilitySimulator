/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.utils.DayType

import scala.util.{Failure, Success, Try}

object FirstDepartureFactory extends ProbabilityFactory[FirstDepartureOfDay] {
  private val uuid = "uuid"
  private val dayType = "day_type"
  private val minute = "minute_of_day"
  private val probability = "probability"
  override protected val requiredFields: Seq[String] =
    Seq(uuid, dayType, minute, probability)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[FirstDepartureOfDay] = {
    val entryTrial = entityFieldData.map(Entry(_))
    entryTrial.find(_.isFailure) match {
      case Some(firstFailure: Failure[Entry]) =>
        Failure(
          SourceException(
            "Unable to build first departure probabilities. First failure in stack trace.",
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
              FirstDepartureOfDay(
                weekdayProbability,
                saturdayProbability,
                sundayProbability,
                round15 = false
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
  ): ProbabilityDensityFunction[Int] =
    ProbabilityDensityFunction(entries.map {
      case Entry(minute, _, probability) => minute -> probability
    }.toMap)

  private final case class Entry(
      minute: Int,
      dayType: DayType.Value,
      probability: Double
  )

  private object Entry {
    def apply(entityFieldData: Map[String, String]): Try[Entry] = Try {
      val minuteValue = entityFieldData
        .getOrElse(
          minute,
          throw SourceException(
            "Unable to get minute information from entity field data"
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
      val probabilityValue = entityFieldData
        .getOrElse(
          probability,
          throw SourceException(
            "Unable to get probability information from entity field data"
          )
        )
        .toDouble

      new Entry(
        minuteValue,
        dayTypeValue,
        probabilityValue
      )
    }
  }
}
