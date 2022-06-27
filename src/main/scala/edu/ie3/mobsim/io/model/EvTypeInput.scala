/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.model

import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.probabilities.ProbabilityDensityFunction
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.interfaces.SpecificEnergy
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import javax.measure.quantity.{Energy, Power}
import scala.io.Source
import scala.util.{Failure, Success, Try, Using}

/** EvInput models which are generated from csv file.
  * @param model
  *   model name
  * @param producer
  *   producer name
  * @param segment
  *   segment type
  * @param capacity
  *   battery capacity
  * @param consumption
  *   battery consumption
  * @param acPower
  *   Possible AC charging power
  * @param dcPower
  *   Possible DC charging power
  */
final case class EvTypeInput(
    model: String,
    producer: String,
    segment: String,
    capacity: ComparableQuantity[Energy],
    consumption: ComparableQuantity[SpecificEnergy],
    acPower: ComparableQuantity[Power],
    dcPower: ComparableQuantity[Power]
)

case object EvTypeInput {

  private def apply(
      evString: String,
      csvSep: String = ","
  ): Try[EvTypeInput] = {
    val entries = evString.trim.toLowerCase.split(csvSep)
    if (entries.length != 7)
      Failure(
        InitializationException(
          s"Received ${entries.length} attributes, but 7 are needed to parse an ev model."
        )
      )
    else
      Try {
        val model = entries(0)
        val producer = entries(1)
        val segment = entries(2)
        val batCap = Quantities.getQuantity(
          entries(3).toDouble,
          PowerSystemUnits.KILOWATTHOUR
        )
        val batCon = Quantities.getQuantity(
          entries(4).toDouble / 100,
          PowerSystemUnits.KILOWATTHOUR_PER_KILOMETRE
        ) // Comes as kWh/km
        val acPower =
          Quantities.getQuantity(entries(5).toDouble, PowerSystemUnits.KILOWATT)
        val dcPower =
          Quantities.getQuantity(entries(6).toDouble, PowerSystemUnits.KILOWATT)

        new EvTypeInput(
          model,
          producer,
          segment,
          batCap,
          batCon,
          acPower,
          dcPower
        )
      }
  }

  /** Determine the probabilities for each available ev model. It is based on
    * the probability of the segment it represents and is evenly distributed
    * across all models in this segment.
    *
    * @param modelFilePath
    *   Path to the file with available ev models
    * @param probabilityFilePath
    *   Path to the file with segment probabilities
    * @return
    *   Probability density function for different ev model types
    */
  def getEvInputModelsWithProbabilities(
      modelFilePath: String,
      probabilityFilePath: String
  ): ProbabilityDensityFunction[EvTypeInput] = {
    val models = getFromFile(modelFilePath)
    getEvInputModelsWithProbabilities(models, probabilityFilePath)
  }

  def getFromFile(
      filePath: String,
      dropFirstLine: Boolean = true
  ): Seq[EvTypeInput] =
    Using(Source.fromFile(filePath)) {
      _.getLines()
        .drop(if (dropFirstLine) 1 else 0)
        .map { inputString =>
          EvTypeInput(inputString) match {
            case Failure(exception) =>
              throw InitializationException(
                s"Unable to parse input string '$inputString' to ev type.",
                exception
              )
            case Success(value) => value
          }
        }
        .toSeq
    } match {
      case Success(value) => value
      case Failure(exception) =>
        throw InitializationException(
          s"Unable to obtain ev models from file '$filePath'.",
          exception
        )
    }

  /** Determine the probabilities for each available ev model. It is based on
    * the probability of the segment it represents and is evenly distributed
    * across all models in this segment.
    *
    * @param modelList
    *   Collection of available ev models
    * @param probabilityFilePath
    *   Path to the file with segment probabilities
    * @return
    *   Probability density function for different ev model types
    */
  def getEvInputModelsWithProbabilities(
      modelList: Seq[EvTypeInput],
      probabilityFilePath: String
  ): ProbabilityDensityFunction[EvTypeInput] = {
    val segmentProbabilities = getEvSegmentProbabilities(probabilityFilePath)

    /* Determine the amount of cars per segment */
    val segmentAppearances = modelList.groupBy(_.segment).map {
      case (segment, evs) => segment -> evs.length
    }

    /* Determine the probability for a given ev model. The probability is defined as the segment probability evenly
     * distributed across all models of this segment. */
    val modelProbability = modelList.map { model =>
      model -> segmentProbabilities
        .get(model.segment)
        .flatMap(segmentProbability =>
          segmentAppearances.get(model.segment).map(segmentProbability / _)
        )
        .getOrElse(0d)
    }.toMap
    ProbabilityDensityFunction(modelProbability)
  }

  /** Read probabilities for each EV segment from csv file
    *
    * @param filePath
    *   Path to the file with probabilities
    * @param csvSep
    *   csv column separator
    * @return
    *   Map with probability for each EV segment
    */
  private def getEvSegmentProbabilities(
      filePath: String,
      csvSep: String = ","
  ): Map[String, Double] =
    Using(Source.fromFile(filePath)) { bufferedSource =>
      val segmentProbabilities = bufferedSource
        .getLines()
        .drop(1)
        .map { inputString =>
          val cols = inputString.split(",").map(_.trim)
          cols(0).toLowerCase -> cols(1).toDouble
        }
        .toMap
      bufferedSource.close

      segmentProbabilities
    } match {
      case Failure(exception) =>
        throw InitializationException(
          "Unable to read ev type segment probabilities.",
          exception
        )
      case Success(value) => value
    }
}
