/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.io.probabilities.DrivingSpeed.SpeedFunction
import edu.ie3.mobsim.io.probabilities.DrivingSpeed
import edu.ie3.mobsim.utils.DayType
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import javax.measure.quantity.Speed
import scala.util.{Failure, Success, Try}

object DrivingSpeedFactory extends ProbabilityFactory[DrivingSpeed] {
  private val uuid = "uuid"
  private val time = "time"
  private val dayType = "day_type"
  private val a = "a"
  private val b = "b"
  private val min = "min"
  override protected val requiredFields: Seq[String] =
    Seq(uuid, time, dayType, a, b, min)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[DrivingSpeed] = {
    val entryTrial = entityFieldData.map(Entry(_))
    entryTrial.find(_.isFailure) match {
      case Some(firstFailure: Failure[Entry]) =>
        Failure(
          SourceException(
            "Unable to build driving speed information. First failure in stack trace.",
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
          .map(entries2speedFunction)
        val saturday = dayTypeToEntry
          .get(DayType.SATURDAY)
          .map(entries2speedFunction)
        val sunday = dayTypeToEntry
          .get(DayType.SUNDAY)
          .map(entries2speedFunction)

        weekday.zip(saturday).zip(sunday) match {
          case Some(
                ((weekdayParameters, saturdayParameters), sundayParameters)
              ) =>
            Success(
              DrivingSpeed(
                weekdayParameters,
                saturdayParameters,
                sundayParameters
              )
            )
          case None =>
            Failure(
              SourceException("Unable to determine all day type parameters.")
            )
        }
    }
  }

  private def entries2speedFunction(
      entries: Seq[Entry]
  ): Map[Int, SpeedFunction] =
    entries.map { case Entry(timeInterval, _, a, b, min) =>
      timeInterval -> SpeedFunction(a, b, min)
    }.toMap

  private final case class Entry(
      timeInterval: Int,
      dayType: DayType.Value,
      a: Double,
      b: Double,
      min: ComparableQuantity[Speed]
  )

  private object Entry {
    def apply(entityFieldData: Map[String, String]): Try[Entry] = Try {
      val timeValue = entityFieldData
        .getOrElse(
          time,
          throw SourceException(
            "Unable to get time interval information from entity field data"
          )
        ) match {
        case "00:00-02:00" => 0
        case "02:00-04:00" => 1
        case "04:00-06:00" => 2
        case "06:00-08:00" => 3
        case "08:00-10:00" => 4
        case "10:00-12:00" => 5
        case "12:00-14:00" => 6
        case "14:00-16:00" => 7
        case "16:00-18:00" => 8
        case "18:00-20:00" => 9
        case "20:00-22:00" => 10
        case "22:00-24:00" => 11
        case illegal =>
          throw SourceException(
            s"Cannot parse '$illegal' to sensible interval of the day."
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
      val aValue = entityFieldData
        .getOrElse(
          a,
          throw SourceException(
            "Unable to get 'a' parameter from entity field data"
          )
        )
        .toDouble
      val bValue = entityFieldData
        .getOrElse(
          b,
          throw SourceException(
            "Unable to get 'b' parameter from entity field data"
          )
        )
        .toDouble
      val minSpeed = Quantities.getQuantity(
        entityFieldData
          .getOrElse(
            min,
            throw SourceException(
              "Unable to get minimum speed information from entity field data"
            )
          )
          .toDouble,
        Units.KILOMETRE_PER_HOUR
      )

      new Entry(
        timeValue,
        dayTypeValue,
        aValue,
        bValue,
        minSpeed
      )
    }
  }
}
