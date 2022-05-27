/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.probabilities.TripDistance.TripDistanceKey
import edu.ie3.mobsim.io.probabilities.{
  ProbabilityDensityFunction,
  TripDistance
}
import edu.ie3.mobsim.utils.DayType
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.Length
import scala.util.{Failure, Success, Try}

object TripDistanceFactory extends ProbabilityFactory[TripDistance] {
  private val uuid = "uuid"
  private val dayType = "day_type"
  private val quarterHour = "quarter_hour_of_day"
  private val fromPoi = "from"
  private val toPoi = "to"
  private val distance = "distance"
  private val probability = "probability"
  override protected val requiredFields: Seq[String] =
    Seq(uuid, dayType, quarterHour, fromPoi, toPoi, distance, probability)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[TripDistance] = {
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
              TripDistance(
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
  ): Map[TripDistanceKey, ProbabilityDensityFunction[Double]] =
    entries
      .groupBy(entry =>
        TripDistanceKey(entry.quarterHour, entry.from.id, entry.to.id)
      )
      .map { case (key, entries) =>
        key -> ProbabilityDensityFunction(entries.map { entry =>
          entry.distance
            .to(PowerSystemUnits.KILOMETRE)
            .getValue
            .doubleValue() -> entry.probability
        }.toMap)
      }

  private final case class Entry(
      quarterHour: Int,
      dayType: DayType.Value,
      from: PoiTypeDictionary.Value,
      to: PoiTypeDictionary.Value,
      distance: ComparableQuantity[Length],
      probability: Double
  )

  private object Entry {
    def apply(entityFieldData: Map[String, String]): Try[Entry] = Try {
      val quarterHourValue = entityFieldData
        .getOrElse(
          quarterHour,
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
      val fromPoiValue = PoiTypeDictionary(
        entityFieldData
          .getOrElse(
            fromPoi,
            throw SourceException(
              "Unable to get source poi type information from entity field data"
            )
          )
      )
      val toPoiValue = PoiTypeDictionary(
        entityFieldData
          .getOrElse(
            toPoi,
            throw SourceException(
              "Unable to get target poi type information from entity field data"
            )
          )
      )
      val distanceValue = Quantities.getQuantity(
        entityFieldData
          .getOrElse(
            distance,
            throw SourceException(
              "Unable to get distance information from entity field data"
            )
          )
          .toDouble,
        PowerSystemUnits.KILOMETRE
      )
      val probabilityValue = entityFieldData
        .getOrElse(
          probability,
          throw SourceException(
            "Unable to get probability information from entity field data"
          )
        )
        .toDouble

      new Entry(
        quarterHourValue,
        dayTypeValue,
        fromPoiValue,
        toPoiValue,
        distanceValue,
        probabilityValue
      )
    }
  }
}
