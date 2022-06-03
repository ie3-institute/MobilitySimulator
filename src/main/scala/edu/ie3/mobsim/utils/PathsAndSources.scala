/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.{RawGridSource, SystemParticipantSource}
import edu.ie3.datamodel.io.source.csv.{
  CsvRawGridSource,
  CsvSystemParticipantSource,
  CsvThermalSource,
  CsvTypeSource
}
import edu.ie3.mobsim.config.MobSimConfig
import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input
import org.apache.commons.io.FilenameUtils
import org.apache.commons.io.filefilter.DirectoryFileFilter.DIRECTORY

import java.io.{File, FileFilter}
import java.nio.file.Paths

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
    outputDir: String,
    rawGridSource: RawGridSource,
    systemParticipantSource: SystemParticipantSource,
    colSep: String
)

object PathsAndSources extends LazyLogging {

  /* Base path */
  private val basePath: String = Paths.get("").toAbsolutePath.toString

  private final case class InitData(
      simulationName: String,
      grid: InitData.Grid,
      mobility: InitData.Mobility
  )
  private object InitData {
    final case class Grid(name: String, path: String, colSep: String)
    final case class Mobility(path: String, colSep: String)
  }

  def apply(
      simulationName: String,
      inputConfig: MobSimConfig.Mobsim.Input
  ): PathsAndSources = {
    val initData = inputConfig match {
      case Input(
            Input.Grid(gridName, CsvParams(gridColSep, gridPath)),
            Input.Mobility(CsvParams(mobColSep, mobPath))
          ) =>
        InitData(
          simulationName,
          InitData
            .Grid(gridName, harmonizeFileSeparators(gridPath), gridColSep),
          InitData.Mobility(harmonizeFileSeparators(mobPath), mobColSep)
        )
    }
    PathsAndSources(initData)
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

  /** Build relevant paths and sources from given init data
    *
    * @param initData
    *   Initialization data
    * @return
    *   An instance of [[PathsAndSources]]
    */
  def apply(initData: InitData): PathsAndSources = initData match {
    case InitData(
          simulationName,
          InitData.Grid(gridName, gridDir, gridColSep),
          InitData.Mobility(mobSimDir, mobColSep)
        ) =>
      /* Build the different directory paths */
      val mobSimInputDir = Seq(basePath, mobSimDir).mkString(File.separator)
      val poiPath =
        Seq(mobSimInputDir, "poi", gridName, "poi.csv").mkString(File.separator)
      val evModelPath =
        Seq(mobSimInputDir, "ev_models").mkString(File.separator)
      val evInputModelPath =
        Seq(evModelPath, "ev_models.csv").mkString(File.separator)
      val evSegmentPath =
        Seq(evModelPath, "segment_probabilities.csv").mkString(File.separator)
      val probabilitiesPath =
        Seq(mobSimInputDir, "trip_probabilities").mkString(File.separator)
      val categoricalLocationPath =
        Seq(probabilitiesPath, "categorical_location.csv")
          .mkString(File.separator)
      val drivingSpeedPath =
        Seq(probabilitiesPath, "driving_speed.csv").mkString(
          File.separator
        )
      val firstDepartureOfDayPath =
        Seq(probabilitiesPath, "departure.csv").mkString(
          File.separator
        )
      val lastTripPath =
        Seq(probabilitiesPath, "last_trip.csv").mkString(
          File.separator
        )
      val parkingTimePath =
        Seq(probabilitiesPath, "parking_time.csv").mkString(
          File.separator
        )
      val poiTransitionPath =
        Seq(probabilitiesPath, "transition.csv")
          .mkString(File.separator)
      val tripDistancePath =
        Seq(probabilitiesPath, "trip_distance.csv").mkString(
          File.separator
        )
      val baseOutputDir =
        Seq(basePath, "output", simulationName).mkString(File.separator)
      val outputDir = determineRecentOutputDirectory(baseOutputDir)

      /* Build the actual sources */
      // TODO: Consider for hierarchic directory structure
      val powerSystemModelDir = Seq(basePath, gridDir).mkString(File.separator)
      val fileNamingStrategy = new FileNamingStrategy()
      val typeSource =
        new CsvTypeSource(gridColSep, powerSystemModelDir, fileNamingStrategy)
      val thermalSource = new CsvThermalSource(
        gridColSep,
        powerSystemModelDir,
        fileNamingStrategy,
        typeSource
      )
      val csvRawGridSource = new CsvRawGridSource(
        gridColSep,
        powerSystemModelDir,
        fileNamingStrategy,
        typeSource
      )

      val systemParticipantSource = new CsvSystemParticipantSource(
        gridColSep,
        powerSystemModelDir,
        fileNamingStrategy,
        typeSource,
        thermalSource,
        csvRawGridSource
      )

      new PathsAndSources(
        mobSimInputDir,
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
        outputDir,
        csvRawGridSource,
        systemParticipantSource,
        mobColSep
      )
  }

  /** Determine the most recent directory within the base output directory. If
    * none can be found, directly write to the base directory
    *
    * @param baseOutputDir
    *   The base output directory
    * @return
    *   The actual target directory
    */
  private def determineRecentOutputDirectory(baseOutputDir: String): String = {
    /* find last modified directory in output path -> this is where SIMONA writes into during the simulation */
    val dir = Paths
      .get(baseOutputDir)
      .toFile
      .listFiles(DIRECTORY.asInstanceOf[FileFilter])
      .maxByOption(_.lastModified())
      .getOrElse {
        logger.warn(
          "Unable to determine most recent output directory. Write to base path!"
        )
        baseOutputDir
      }
    Seq(dir, "mobilitySimulator").mkString(File.separator)
  }
}
