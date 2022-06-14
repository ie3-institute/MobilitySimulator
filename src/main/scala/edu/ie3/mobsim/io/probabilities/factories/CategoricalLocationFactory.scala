/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.probabilities.CategoricalLocation.CategoricalLocationKey
import edu.ie3.mobsim.io.probabilities.{
  CategoricalLocation,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.utils.DayType

import scala.util.{Failure, Success, Try}

object CategoricalLocationFactory
    extends ProbabilityFactory[CategoricalLocation] {
  private val uuid = "uuid"
  private val time = "time"
  private val dayType = "day_type"
  private val poiType = "poi_type"
  private val categoricalLocation = "categorical_location"
  private val probability = "probability"
  override protected val requiredFields: Seq[String] =
    Seq(uuid, time, dayType, poiType, categoricalLocation, probability)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[CategoricalLocation] = {
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
              CategoricalLocation(
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
  ): Map[CategoricalLocationKey, ProbabilityDensityFunction[
    CategoricalLocationDictionary.Value
  ]] =
    entries
      .groupBy(entry =>
        CategoricalLocationKey(
          entry.time,
          entry.poiType
        )
      )
      .map { case (key, entries) =>
        key -> ProbabilityDensityFunction(entries.map { entry =>
          entry.categoricalLocation -> entry.probability
        }.toMap)
      }

  private final case class Entry(
      time: Int,
      dayType: DayType.Value,
      poiType: PoiTypeDictionary.Value,
      categoricalLocation: CategoricalLocationDictionary.Value,
      probability: Double
  )

  private object Entry {
    def apply(entityFieldData: Map[String, String]): Try[Entry] = Try {
      val timeValue = entityFieldData
        .getOrElse(
          time,
          throw SourceException(
            "Unable to get minute information from entity field data"
          )
        ) match {
        case "5:00-10:00"  => 0
        case "10:00-12:00" => 1
        case "12:00-14:00" => 2
        case "14:00-18:00" => 3
        case "18:00-23:00" => 4
        case "23:00-5:00"  => 5
        case illegal =>
          throw SourceException(
            s"Cannot parse sensible interval in day from '$illegal'."
          )
      }
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
        entityFieldData
          .getOrElse(
            poiType,
            throw SourceException(
              "Unable to get poi type information from entity field data"
            )
          )
      )
      val categoricalLocationValue = CategoricalLocationDictionary(
        entityFieldData
          .getOrElse(
            categoricalLocation,
            throw SourceException(
              "Unable to get categorical location information from entity field data"
            )
          )
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
        timeValue,
        dayTypeValue,
        poiTypeValue,
        categoricalLocationValue,
        probabilityValue
      )
    }
  }
}
