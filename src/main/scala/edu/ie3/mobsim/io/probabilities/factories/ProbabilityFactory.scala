/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.exceptions.SourceException

import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try, Using}

trait ProbabilityFactory[T] {
  protected val requiredFields: Seq[String]

  /** Get the desired information from a given csv file
    *
    * @param filePath
    *   Path of the file
    * @param csvSep
    *   Column separator
    * @return
    *   A trial to get the desired instance
    */
  def getFromFile(filePath: String, csvSep: String): Try[T] =
    Using(Source.fromFile(filePath).bufferedReader()) { reader =>
      checkHeadLine(reader.readLine(), csvSep).flatMap { headLineElements =>
        val entityFieldData: Seq[Map[String, String]] = reader
          .lines()
          .map { line =>
            headLineElements.zip(processLine(line, csvSep)).toMap
          }
          .toList
          .asScala
          .toSeq
        build(entityFieldData)
      }
    }.flatten

  /** Check, if the headline meets the requirements and hand back the field
    * information
    *
    * @param headLine
    *   The headline of the file
    * @param csvSep
    *   Column separator
    * @return
    *   Headline elements, if check is successful
    */
  protected def checkHeadLine(
      headLine: String,
      csvSep: String,
  ): Try[Array[String]] = {
    val availableElements = processLine(headLine, csvSep)
    if (availableElements.sorted.sameElements(requiredFields.sorted))
      Success(availableElements)
    else
      Failure(
        SourceException(s"Unable to read from content from file. Available headline elements: '${availableElements
            .mkString(",")}', required fields: '${requiredFields.mkString(",")}'")
      )
  }

  /** Process a single line
    *
    * @param line
    *   The line, that is read from file
    * @param csvSep
    *   Column separator
    * @return
    *   An [[Array]] of field values
    */
  protected def processLine(line: String, csvSep: String): Array[String] =
    line.toLowerCase.split(csvSep).map(_.trim)

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  protected def build(entityFieldData: Seq[Map[String, String]]): Try[T]
}
