/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.datamodel.io.csv.BufferedCsvWriter

import java.io.{File => JavaFile}
import java.nio.file.Path
import java.util.UUID
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.reflect.io.File
import scala.util.{Failure, Success, Try, Using}

object FileChanger extends App {
  private final case class SourceDefinition(
      sourcePath: String,
      sourceHeader: Seq[String],
      fieldNameMapping: Map[String, String],
      additionalDataFunction: () => Map[String, String],
  )

  private val csvSep = ","
  private val baseDir =
    Seq("input", "mobilitySimulator").mkString(JavaFile.separator)
  private val poiDir = Seq(baseDir, "poi").mkString(JavaFile.separator)
  private val probabilityDir =
    Seq(baseDir, "trip_probabilities").mkString(JavaFile.separator)

  changePoiFile()
  mergeDepartures()
  mergeParkingTime()
  mergeDrivingSpeed()
  mergeCategoricalLocations()
  mergeTransitionProbabilities()
  mergeTripDistances()
  mergeLastTrip()

  private def changePoiFile(): Unit = {
    val sourceDefinition = SourceDefinition(
      Seq(poiDir, "pcm", "poi.csv").mkString(JavaFile.separator),
      Seq("id", "size", "lat", "lon", "categoricallocation"),
      Map.empty[String, String],
      () => Map("uuid" -> UUID.randomUUID().toString),
    )

    val targetPath =
      Path.of(poiDir, "pcm", "poi_altered.csv")
    val targetHeader =
      Seq("uuid", "id", "size", "lat", "lon", "categoricallocation")

    mergeFiles(
      Seq(sourceDefinition),
      targetPath,
      targetHeader,
    )
  }

  private def mergeDepartures(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Abfahrtszeit_Werktag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time", "probabilities"),
        Map("time" -> "minute_of_day", "probabilities" -> "probability"),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Abfahrtszeit_Samstag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time", "probabilities"),
        Map("time" -> "minute_of_day", "probabilities" -> "probability"),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Abfahrtszeit_Sonn - Feiertag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time", "probabilities"),
        Map("time" -> "minute_of_day", "probabilities" -> "probability"),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "departure.csv")
    val targetHeader =
      Seq("uuid", "day_type", "minute_of_day", "probability")

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeParkingTime(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Aufenthaltsdauer_Werktag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "poi", "parking_time", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "poi" -> "poi_type",
          "parking_time" -> "duration",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Aufenthaltsdauer_Samstag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "poi", "parking_time", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "poi" -> "poi_type",
          "parking_time" -> "duration",
          "probabilities" -> "probability",
        ),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Aufenthaltsdauer_Sonn - Feiertag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "poi", "parking_time", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "poi" -> "poi_type",
          "parking_time" -> "duration",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "parking_time.csv")
    val targetHeader =
      Seq(
        "uuid",
        "day_type",
        "quarter_hour_of_day",
        "poi_type",
        "duration",
        "probability",
      )

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeDrivingSpeed(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Geschwindigkeit_Werktag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "a", "b", "min"),
        Map("time_interval" -> "time"),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Geschwindigkeit_Samstag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "a", "b", "min"),
        Map("time_interval" -> "time"),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Geschwindigkeit_Sonn - Feiertag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "a", "b", "min"),
        Map("time_interval" -> "time"),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "driving_speed.csv")
    val targetHeader =
      Seq("uuid", "day_type", "time", "a", "b", "min")

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeCategoricalLocations(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Kategorische_Aufenthaltsorte_Werktag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "poi", "categ_location", "probabilities"),
        Map(
          "time_interval" -> "time",
          "poi" -> "poi_type",
          "categ_location" -> "categorical_location",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Kategorische_Aufenthaltsorte_Samstag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "poi", "categ_location", "probabilities"),
        Map(
          "time_interval" -> "time",
          "poi" -> "poi_type",
          "categ_location" -> "categorical_location",
          "probabilities" -> "probability",
        ),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Kategorische_Aufenthaltsorte_Sonn - Feiertag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "poi", "categ_location", "probabilities"),
        Map(
          "time_interval" -> "time",
          "poi" -> "poi_type",
          "categ_location" -> "categorical_location",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath = Path.of(probabilityDir, "categorical_location.csv")
    val targetHeader =
      Seq(
        "uuid",
        "day_type",
        "time",
        "poi_type",
        "categorical_location",
        "probability",
      )

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeTransitionProbabilities(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Uebergangswahrscheinlichkeit_Werktag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "from_poi", "to_poi", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Uebergangswahrscheinlichkeit_Samstag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "from_poi", "to_poi", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Uebergangswahrscheinlichkeit_Sonn - Feiertag.csv")
          .mkString(JavaFile.separator),
        Seq("time_interval", "from_poi", "to_poi", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "transition.csv")
    val targetHeader =
      Seq(
        "uuid",
        "day_type",
        "quarter_hour_of_day",
        "from",
        "to",
        "probability",
      )

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeTripDistances(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Wegstrecken_Werktag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "from_poi", "to_poi", "distance", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Wegstrecken_Samstag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "from_poi", "to_poi", "distance", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Wegstrecken_Sonn - Feiertag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "from_poi", "to_poi", "distance", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "from_poi" -> "from",
          "to_poi" -> "to",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "trip_distance.csv")
    val targetHeader =
      Seq(
        "uuid",
        "day_type",
        "quarter_hour_of_day",
        "from",
        "to",
        "distance",
        "probability",
      )

    mergeFiles(sources, targetPath, targetHeader)
  }

  private def mergeLastTrip(): Unit = {
    val sources = Seq(
      SourceDefinition(
        Seq(probabilityDir, "Zwischenstopp_Werktag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "weekday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Zwischenstopp_Samstag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "probabilities" -> "probability",
        ),
        () =>
          Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "saturday"),
      ),
      SourceDefinition(
        Seq(probabilityDir, "Zwischenstopp_Sonn - Feiertag.csv").mkString(
          JavaFile.separator
        ),
        Seq("time_interval", "probabilities"),
        Map(
          "time_interval" -> "quarter_hour_of_day",
          "probabilities" -> "probability",
        ),
        () => Map("uuid" -> UUID.randomUUID().toString, "day_type" -> "sunday"),
      ),
    )

    val targetPath =
      Path.of(probabilityDir, "last_trip.csv")
    val targetHeader =
      Seq("uuid", "day_type", "quarter_hour_of_day", "probability")

    mergeFiles(sources, targetPath, targetHeader)
  }

  /** Adapt and merge the content of several source files
    *
    * @param sources
    *   Definition of sources
    * @param targetPath
    *   Target file path
    * @param targetHeader
    *   Header of the target file
    */
  private def mergeFiles(
      sources: Seq[SourceDefinition],
      targetPath: Path,
      targetHeader: Seq[String],
  ): Unit = {
    val targetFile = File(targetPath.toFile)
    if (!targetFile.exists)
      targetFile.createFile()

    Using(
      new BufferedCsvWriter(targetPath, targetHeader.toArray, csvSep, false)
    ) { writer =>
      writer.writeFileHeader()

      sources
        .flatMap {
          case SourceDefinition(
                sourcePath,
                sourceHeader,
                fieldNameMapping,
                additionalDataFunction,
              ) =>
            transformData(
              sourcePath,
              sourceHeader,
              fieldNameMapping,
              additionalDataFunction,
            ) match {
              case Failure(exception) =>
                throw new RuntimeException(
                  s"Unable to parse content of file '$sourcePath'.",
                  exception,
                )
              case Success(value) => value
            }
        }
        .foreach(entityFieldData => writer.write(entityFieldData.asJava))
    }
  }

  /** Check, if the headline is correct and give back a mapping from field name
    * to column index
    *
    * @param headLine
    *   Headline to check
    * @param expectedElements
    *   Expected elements
    * @return
    *   Mapping from field name to column index
    */
  private def assessHeadLineElements(
      headLine: String,
      expectedElements: Seq[String],
  ): Map[String, Int] = {
    val elements = headLine.toLowerCase.split(csvSep).map(_.trim)
    if (!elements.sameElements(expectedElements))
      throw new RuntimeException(
        s"Unable to parse input file. Malicious header: Apparent = ${elements
            .mkString(", ")}, expected = ${expectedElements.mkString(", ")}"
      )

    elements.zipWithIndex.toMap
  }

  /** Read in data and transform them to the target data
    *
    * @param sourcePath
    *   Path of the source file
    * @param sourceHeader
    *   Head line elements of the source
    * @param fieldNameMapping
    *   Mapping from source to target field name
    * @param additionalDataFunction
    *   Function to add missing information
    * @return
    *   A sequence of entity field data
    */
  private def transformData(
      sourcePath: String,
      sourceHeader: Seq[String],
      fieldNameMapping: Map[String, String],
      additionalDataFunction: () => Map[String, String],
  ): Try[Seq[Map[String, String]]] =
    Using(Source.fromFile(sourcePath).bufferedReader()) { reader =>
      /* Prepare information from reader */
      val fieldToIndex =
        assessHeadLineElements(reader.readLine(), sourceHeader)

      reader
        .lines()
        .map {
          processInputLine(
            _,
            fieldToIndex,
            fieldNameMapping,
            additionalDataFunction,
          )
        }
        .toList
        .asScala
        .toSeq
    }

  /** Process a single line of input data
    *
    * @param line
    *   One line of the input file
    * @param fieldToIndex
    *   Mapping from field name to column index
    * @param fieldNameMapping
    *   Mapping from input field name to output field name
    * @param additionalDataFunction
    *   Function to add missing information
    */
  private def processInputLine(
      line: String,
      fieldToIndex: Map[String, Int],
      fieldNameMapping: Map[String, String],
      additionalDataFunction: () => Map[String, String],
  ): Map[String, String] = {
    /* Read the content of the line */
    val elements = line.split(csvSep).map(_.trim)
    val entityFieldData = fieldToIndex.map { case (fieldName, index) =>
      fieldName -> elements(index)
    }

    /* Map the field names */
    val updatedEntityFieldData =
      entityFieldData.map { case (key, value) =>
        fieldNameMapping.getOrElse(key, key) -> {
          value match {
            case "othershop"          => "other_shop"
            case "charginghubtown"    => "charging_hub_town"
            case "charginghubhighway" => "charging_hub_highway"
            case _                    => value
          }
        }
      }

    /* Add missing data */
    val additionalData = additionalDataFunction()
    updatedEntityFieldData ++ additionalData
  }
}
