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
    poiPath: Path,
    homePoiMappingPath: Path,
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
    csvSep: String,
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
    val mobSimDir = harmonizeFileSeparators(inputConfig.inputDir)
    val mobSimInputDir = simonaInputDir.resolve(mobSimDir)

    val (
      poiPath,
      homePoiMappingPath,
      evInputModelPath,
      evSegmentPath,
      categoricalLocationPath,
      drivingSpeedPath,
      firstDepartureOfDayPath,
      lastTripPath,
      parkingTimePath,
      poiTransitionPath,
      tripDistancePath,
    ) = if inputConfig.hierarchic then {
      /* Build the different directory paths */
      val poiPath = mobSimInputDir.resolve("poi")
      val probabilitiesPath = mobSimInputDir.resolve("trip_probabilities")
      val evModelPath = mobSimInputDir.resolve("ev_models")

      (
        poiPath.resolve("poi.csv"),
        poiPath.resolve("poi_mapping.csv"),
        evModelPath.resolve("ev_models.csv").toString,
        evModelPath.resolve("segment_probabilities.csv").toString,
        probabilitiesPath.resolve("categorical_location.csv").toString,
        probabilitiesPath.resolve("driving_speed.csv").toString,
        probabilitiesPath.resolve("departure.csv").toString,
        probabilitiesPath.resolve("last_trip.csv").toString,
        probabilitiesPath.resolve("parking_time.csv").toString,
        probabilitiesPath.resolve("transition.csv").toString,
        probabilitiesPath.resolve("trip_distance.csv").toString,
      )
    } else {
      (
        mobSimInputDir.resolve("poi.csv"),
        mobSimInputDir.resolve("poi_mapping.csv"),
        mobSimInputDir.resolve("ev_models.csv").toString,
        mobSimInputDir.resolve("segment_probabilities.csv").toString,
        mobSimInputDir.resolve("categorical_location.csv").toString,
        mobSimInputDir.resolve("driving_speed.csv").toString,
        mobSimInputDir.resolve("departure.csv").toString,
        mobSimInputDir.resolve("last_trip.csv").toString,
        mobSimInputDir.resolve("parking_time.csv").toString,
        mobSimInputDir.resolve("transition.csv").toString,
        mobSimInputDir.resolve("trip_distance.csv").toString,
      )
    }

    new PathsAndSources(
      mobSimInputDir.toString,
      poiPath,
      homePoiMappingPath,
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
      inputConfig.csvSep,
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
}
