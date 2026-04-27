/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.mobsim.config.MobSimConfig
import org.apache.commons.io.FilenameUtils

import java.nio.file.{Files, Path, Paths}
import scala.jdk.CollectionConverters.*

final case class PathsAndSources private (
    mobSimInputDir: String,
    poiPath: String,
    evInputModelPath: String,
    evSegmentPath: String,
    categoricalLocationPath: String,
    drivingSpeedPath: String,
    firstDepartureOfDayPath: String,
    lastTripPath: String,
    parkingTimePath: String,
    poiTransitionPath: String,
    tripDistancePath: String,
    outputDir: Path,
    colSep: String,
)

/** Beware: Since the simulation is executed as a jar within SIMONA relative
  * paths are set according to your SIMONA project path. Furthermore, if the
  * output path is not configured it will be placed within the output simulation
  * folder of the SIMONA run you are executing.
  */
object PathsAndSources extends LazyLogging {

  def apply(
      simulationName: String,
      inputConfig: MobSimConfig.Input,
      simonaInputDir: Path,
      simonaOutputDir: Path,
      outputDirName: String,
  ): PathsAndSources = {
    val mobSimDir = harmonizeFileSeparators(inputConfig.mobility.path)

    /* Build the different directory paths */
    val mobSimDirPath = Paths.get(mobSimDir)
    val mobSimInputDir =
      if (mobSimDirPath.isAbsolute) mobSimDirPath
      else simonaInputDir.resolve(mobSimDir)
    val poiPath = {
      mobSimInputDir
        .resolve("poi")
        .resolve("poi.csv")
        .toString
    }

    val evModelPath =
      mobSimInputDir.resolve("ev_models")
    val evInputModelPath =
      evModelPath.resolve("ev_models.csv").toString
    val evSegmentPath =
      evModelPath.resolve("segment_probabilities.csv").toString

    val probabilitiesPath =
      mobSimInputDir.resolve("trip_probabilities")
    val categoricalLocationPath =
      probabilitiesPath.resolve("categorical_location.csv").toString
    val drivingSpeedPath =
      probabilitiesPath.resolve("driving_speed.csv").toString
    val firstDepartureOfDayPath =
      probabilitiesPath.resolve("departure.csv").toString
    val lastTripPath =
      probabilitiesPath.resolve("last_trip.csv").toString
    val parkingTimePath =
      probabilitiesPath.resolve("parking_time.csv").toString
    val poiTransitionPath =
      probabilitiesPath.resolve("transition.csv").toString
    val tripDistancePath =
      probabilitiesPath.resolve("trip_distance.csv").toString

    new PathsAndSources(
      mobSimInputDir.toString,
      poiPath,
      evInputModelPath,
      evSegmentPath,
      categoricalLocationPath,
      drivingSpeedPath,
      firstDepartureOfDayPath,
      lastTripPath,
      parkingTimePath,
      poiTransitionPath,
      tripDistancePath,
      simonaOutputDir.resolve(outputDirName),
      inputConfig.mobility.colSep,
    )

  }

  /** Convert all apparent file separators to the local system's separator
    * character and remove the trailing separator
    *
    * @param path
    *   The path to manipulate
    * @return
    *   The path with harmonized file separators
    */
  private def harmonizeFileSeparators(path: String): String =
    FilenameUtils.normalizeNoEndSeparator(path)

  /** Determine the most recent directory within the base output directory. If
    * none can be found, directly write to the base directory.
    *
    * @param baseOutputDir
    *   The base output directory
    * @return
    *   The actual target directory
    */
  private def determineRecentOutputDirectory(baseOutputDir: String): String = {
    /* find last modified directory in output path -> this is where SIMONA writes into during the simulation */
    val outputDir = Paths.get(baseOutputDir)

    val dir =
      if (!Files.exists(outputDir)) outputDir
      else {
        Files
          .newDirectoryStream(outputDir)
          .asScala
          .filter(Files.isDirectory(_))
          .maxByOption(Files.getLastModifiedTime(_))
          .getOrElse {
            logger.warn(
              "Unable to determine most recent output directory. Write to base path!"
            )
            outputDir
          }
      }
    dir.resolve("mobilitySimulator").toString
  }
}
