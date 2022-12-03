/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.datamodel.io.csv.BufferedCsvWriter
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.csv.{
  CsvRawGridSource,
  CsvSystemParticipantSource,
  CsvThermalSource,
  CsvTypeSource
}
import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.model.{ChargingStation, ElectricVehicle}
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOWATT,
  KILOWATTHOUR,
  KILOWATTHOUR_PER_KILOMETRE
}
import kantan.csv.{RowDecoder, _}
import kantan.csv.ops.toCsvInputOps

import java.io.{File, IOException}
import java.nio.file.{Files, Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class IoUtils private (
    movementWriter: BufferedCsvWriter,
    evWriter: BufferedCsvWriter,
    evcsWriter: BufferedCsvWriter,
    evPosWriter: BufferedCsvWriter,
    poiWriter: BufferedCsvWriter,
    csvSep: String
) {

  /** Define relevant data for EV and call IO function to write it to csv.
    *
    * @param ev
    *   EV
    * @param currentTime
    *   current time
    * @param status
    *   If the car arrives or departs
    * @param uuid
    *   Unique identifier fot the entry
    */
  def writeMovement(
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      status: String,
      uuid: UUID = UUID.randomUUID()
  ): Unit = {
    val fieldData = Map(
      "uuid" -> uuid.toString,
      "ev" -> ev.getUuid.toString,
      "status" -> status,
      "soc" -> ev.getStoredEnergy
        .divide(ev.getEStorage)
        .getValue
        .doubleValue()
        .toString,
      "destination_poi" -> ev.destinationPoi.id,
      "destination_poi_type" -> ev.destinationPoiType.toString,
      "categorical_location" -> ev.destinationPoi.categoricalLocation.toString,
      "destination_arrival" -> ev.parkingTimeStart.toString,
      "destination_departure" -> ev.departureTime.toString,
      "is_charging" -> ev.chargingAtSimona.toString
    ).asJava

    movementWriter.write(fieldData)
  }

  /** Write ev details to csv file
    *
    * @param electricVehicles
    *   Collection of electric vehicles
    */
  def writeEvs(
      electricVehicles: Iterable[ElectricVehicle]
  ): Unit = electricVehicles.foreach { ev =>
    val fieldData = Map(
      "uuid" -> ev.getUuid.toString,
      "id" -> ev.getId,
      "model" -> ev.evType.model,
      "battery_capacity" ->
        ev.getEStorage.to(KILOWATTHOUR).getValue.doubleValue().toString,
      "max_charging_power_ac" ->
        ev.getSRatedAC.to(KILOWATT).getValue.doubleValue().toString,
      "max_charging_power_dc" ->
        ev.getSRatedDC.to(KILOWATT).getValue.doubleValue().toString,
      "consumption" ->
        ev.evType.consumption
          .to(KILOWATTHOUR_PER_KILOMETRE)
          .getValue
          .doubleValue()
          .toString,
      "home_poi" -> ev.homePoi.id,
      "work_poi" -> ev.workPoi.id,
      "is_home_charging_possible" -> ev.chargingAtHomePossible.toString
    ).asJava

    evWriter.write(fieldData)
  }

  /** Define relevant data for charging station and call IO function to write it
    * to csv.
    *
    * @param cs
    *   charging station
    * @param chargingStationOccupancy
    *   current mapping of charging station UUID to parking evs
    * @param currentTime
    *   current time
    * @param uuid
    *   Unique identifier fot the entry
    */
  def writeEvcs(
      cs: ChargingStation,
      chargingStationOccupancy: Map[UUID, Seq[ElectricVehicle]],
      currentTime: ZonedDateTime,
      uuid: UUID = UUID.randomUUID()
  ): Unit = {
    val fieldData = Map(
      "uuid" -> uuid.toString,
      "time" -> currentTime.toString,
      "evcs" -> cs.uuid.toString,
      "charging_points" -> cs.chargingPoints.toString,
      "charging_evs" -> chargingStationOccupancy
        .getOrElse(cs.uuid, Seq.empty)
        .map(_.uuid)
        .mkString("[", "|", "]")
    ).asJava

    evcsWriter.write(fieldData)
  }

  /** Write given POIs to csv file
    *
    * @param pois
    *   POIs to write
    */
  def writePois(
      pois: Map[CategoricalLocationDictionary.Value, Seq[PointOfInterest]]
  ): Unit = pois.foreach { case (poiType, typePois) =>
    typePois.foreach { poi =>
      /* Get all charging stations nearby */
      poi.nearestChargingStations.foreach { case (evcsUuid, distance) =>
        val fieldData = Map(
          "uuid" -> UUID.randomUUID().toString,
          "id" -> poi.id,
          "type" -> poiType.toString,
          "size" -> poi.size.toString,
          "evcs" -> evcsUuid.toString,
          "distance" -> distance.getValue.doubleValue().toString
        ).asJava

        poiWriter.write(fieldData)
      }
    }
  }

  /** Define relevant data for EV and call IO function to write it to csv.
    *
    * @param ev
    *   EV
    * @param time
    *   current time
    * @param uuid
    *   Unique identifier fot the entry
    */
  def writeEvPosition(
      ev: ElectricVehicle,
      time: ZonedDateTime,
      uuid: UUID = UUID.randomUUID()
  ): Unit = {
    val (location, destinationPoi) =
      if (time.isBefore(ev.parkingTimeStart)) {
        ("DRIVING", "")
      } else {
        (
          ev.destinationPoi.categoricalLocation.toString,
          ev.destinationPoi.toString
        )
      }

    val fieldData = Map(
      "uuid" -> uuid.toString,
      "ev" -> ev.getUuid.toString,
      "current_location_type" -> location,
      "destination_poi" -> destinationPoi
    ).asJava

    evPosWriter.write(fieldData)
  }
}

object IoUtils {

  def apply(
      outputPath: String,
      movementFileName: String,
      evFileName: String,
      evcsFileName: String,
      evPosFileName: String,
      poiFileName: String,
      csvSep: String = ";"
  ): IoUtils = {
    Files.createDirectories(Paths.get(outputPath))

    /* Create writer for ev movements and write headline */
    val movementWriter = {
      val filePath = Seq(outputPath, movementFileName).mkString(File.separator)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "ev",
          "status",
          "soc",
          "destination_poi",
          "destination_poi_type",
          "categorical_location",
          "destination_arrival",
          "destination_departure",
          "is_charging"
        ),
        csvSep,
        true
      )
    }
    movementWriter.writeFileHeader()

    /* Create writer for evs and write headline */
    val evWriter = {
      val filePath = Seq(outputPath, evFileName).mkString(File.separator)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "id",
          "model",
          "battery_capacity",
          "max_charging_power_ac",
          "max_charging_power_dc",
          "consumption",
          "home_poi",
          "work_poi",
          "is_home_charging_possible"
        ),
        csvSep,
        true
      )
    }
    evWriter.writeFileHeader()

    /* Create writer for evcs and write headline */
    val evcsWriter = {
      val filePath = Seq(outputPath, evcsFileName).mkString(File.separator)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "time",
          "evcs",
          "charging_points",
          "charging_evs"
        ),
        csvSep,
        true
      )
    }
    evcsWriter.writeFileHeader()

    /* Create writer for ev positions and write headline */
    val evPosWriter = {
      val filePath = Seq(outputPath, evPosFileName).mkString(File.separator)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "ev",
          "current_location_type",
          "destination_poi"
        ),
        csvSep,
        true
      )
    }
    evPosWriter.writeFileHeader()

    /* Create writer for points of interest and write headline */
    val poiWriter = {
      val filePath = Seq(outputPath, poiFileName).mkString(File.separator)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "id",
          "type",
          "size",
          "evcs",
          "distance"
        ),
        csvSep,
        true
      )
    }
    poiWriter.writeFileHeader()

    new IoUtils(
      movementWriter,
      evWriter,
      evcsWriter,
      evPosWriter,
      poiWriter,
      csvSep
    )
  }

  def readEvInputs(csvParams: CsvParams): Seq[EvInput] = {
    val namingStrategy = new FileNamingStrategy()
    val typeSource =
      new CsvTypeSource(csvParams.colSep, csvParams.path, namingStrategy)
    val thermalSource = new CsvThermalSource(
      csvParams.colSep,
      csvParams.path,
      namingStrategy,
      typeSource
    )
    val rawGridSource =
      new CsvRawGridSource(
        csvParams.colSep,
        csvParams.path,
        namingStrategy,
        typeSource
      )
    val systemParticipantSource = new CsvSystemParticipantSource(
      csvParams.colSep,
      csvParams.path,
      namingStrategy,
      typeSource,
      thermalSource,
      rawGridSource
    )
    val evs = systemParticipantSource.getEvs
    if (evs.isEmpty) {
      throw new IOException(s"No evs parsed at ${csvParams.path}!")
    }
    evs.asScala.toSeq
  }

  def getProjectRootDir: String = {
    System.getProperty("user.dir")
  }

  def getAbsolutePath(folderPath: String): Path = {
    val path = Paths.get(folderPath)
    if (!path.isAbsolute) Paths.get(getProjectRootDir, path.toString)
    else path
  }

  def readCaseClassSeq[T](implicit
      decoder: RowDecoder[T],
      folderPath: String,
      csvSep: Char
  ): Seq[T] = {
    val absolutePath = getAbsolutePath(folderPath).toUri
    ReadResult.sequence(
      absolutePath.readCsv[List, T](rfc.withHeader.withCellSeparator(csvSep))
    ) match {
      case Left(readError) => throw readError
      case Right(values)   => values
    }
  }

}
