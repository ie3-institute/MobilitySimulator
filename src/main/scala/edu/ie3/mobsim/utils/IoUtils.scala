/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.datamodel.io.csv.BufferedCsvWriter
import edu.ie3.datamodel.io.naming.FileNamingStrategy
import edu.ie3.datamodel.io.source.*
import edu.ie3.datamodel.io.source.csv.*
import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import squants.space.Kilometers

import java.io.{BufferedReader, FileReader, IOException}
import java.nio.file.{Files, Path, Paths}
import java.time.ZonedDateTime
import java.util.UUID
import scala.jdk.CollectionConverters.*

final case class IoUtils private (
    movementWriter: BufferedCsvWriter,
    evWriter: BufferedCsvWriter,
    evPosWriter: BufferedCsvWriter,
    poiWriter: BufferedCsvWriter,
    csvSep: String,
    writeMovements: Boolean,
) {

  /** Define relevant data for EV and call IO function to write it to csv.
    *
    * @param ev
    *   EV
    * @param currentTime
    *   current time
    */
  def writeMovement(
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
  ): Unit = {
    val fieldData = Map(
      "ev" -> ev.getUuid.toString,
      "currentTime" -> currentTime.toString,
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
        (ev.getSRatedAC
          .to(KILOWATT)
          .getValue
          .doubleValue() * ev.getCosPhiRated).toString,
      "max_charging_power_dc" ->
        ev.getPRatedDC.to(KILOWATT).getValue.doubleValue().toString,
      "consumption" ->
        ev.evType.consumption.toKilowattHoursPerKilometer.toString,
      "home_poi" -> ev.homePoi.id,
      "work_poi" -> ev.workPoi.id,
      "is_home_charging_possible" -> ev.chargingAtHomePossible.toString,
    ).asJava

    evWriter.write(fieldData)
  }

  /** Write given POIs to csv file
    *
    * @param pois
    *   POIs to write
    * @param evcsDirectHomePoiMapping
    *   * POIs of Homes that got directly mapped
    */
  def writePois(
      pois: Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]],
      evcsDirectHomePoiMapping: Map[UUID, UUID],
  ): Unit = pois.foreach { case (poiType, typePois) =>
    typePois.foreach { poi =>
      /* Get all charging stations nearby */

      val nearestStations = poi.nearestChargingStations
      // Write POI data even if there are no nearby charging stations
      if (nearestStations.isEmpty) {

        val fieldData = Map(
          "uuid" -> poi.uuid.toString,
          "id" -> poi.id,
          "type" -> poiType.toString,
          "size" -> poi.size.toString,
          "evcs" -> evcsDirectHomePoiMapping
            .get(poi.uuid)
            .map(_.toString)
            .getOrElse(""),
          "distance" -> Kilometers(0).toString,
        ).asJava

        poiWriter.write(fieldData)
      } else {
        // Write data for each nearby charging station
        nearestStations.foreach { case (evcsUuid, distance) =>
          val fieldData = Map(
            "uuid" -> poi.uuid.toString,
            "id" -> poi.id,
            "type" -> poiType.toString,
            "size" -> poi.size.toString,
            "evcs" -> evcsUuid.toString,
            "distance" -> distance.toKilometers.toString,
          ).asJava

          poiWriter.write(fieldData)
        }
      }
    }
  }

  /** Define relevant data for EV and call IO function to write it to csv.
    *
    * @param ev
    *   EV
    * @param time
    *   current time
    */
  def writeEvPosition(
      ev: ElectricVehicle,
      time: ZonedDateTime,
  ): Unit = {
    val (location, destinationPoi) =
      if (time.isBefore(ev.parkingTimeStart)) {
        ("DRIVING", "")
      } else {
        (
          ev.destinationPoi.categoricalLocation.toString,
          ev.destinationPoi.toString,
        )
      }

    val fieldData = Map(
      "time" -> time.toString,
      "ev" -> ev.getUuid.toString,
      "current_location_type" -> location,
      "destination_poi" -> destinationPoi,
    ).asJava

    evPosWriter.write(fieldData)
  }
}

object IoUtils {

  def apply(
      outputPath: String,
      movementFileName: String,
      evFileName: String,
      evPosFileName: String,
      poiFileName: String,
      writeMovements: Boolean,
      csvSep: String = ";",
  ): IoUtils = {
    Files.createDirectories(Paths.get(outputPath))

    /* Create writer for ev movements and write headline */
    val movementWriter = {
      val filePath = Path.of(outputPath, movementFileName)
      new BufferedCsvWriter(
        filePath,
        Array(
          "ev",
          "currentTime",
          "soc",
          "destination_poi",
          "destination_poi_type",
          "categorical_location",
          "destination_arrival",
          "destination_departure",
        ),
        csvSep,
        true,
      )
    }
    movementWriter.writeFileHeader()

    /* Create writer for evs and write headline */
    val evWriter = {
      val filePath = Path.of(outputPath, evFileName)
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
          "is_home_charging_possible",
        ),
        csvSep,
        true,
      )
    }
    evWriter.writeFileHeader()

    /* Create writer for ev positions and write headline */
    val evPosWriter = {
      val filePath = Path.of(outputPath, evPosFileName)
      new BufferedCsvWriter(
        filePath,
        Array(
          "time",
          "ev",
          "current_location_type",
          "destination_poi",
        ),
        csvSep,
        true,
      )
    }
    evPosWriter.writeFileHeader()

    /* Create writer for points of interest and write headline */
    val poiWriter = {
      val filePath = Path.of(outputPath, poiFileName)
      new BufferedCsvWriter(
        filePath,
        Array(
          "uuid",
          "id",
          "type",
          "size",
          "evcs",
          "distance",
        ),
        csvSep,
        true,
      )
    }
    poiWriter.writeFileHeader()

    new IoUtils(
      movementWriter,
      evWriter,
      evPosWriter,
      poiWriter,
      csvSep,
      writeMovements,
    )
  }

  def readEvInputs(csvParams: CsvParams): Seq[EvInput] = {
    val namingStrategy = new FileNamingStrategy()

    val csvDataSource = new CsvDataSource(
      csvParams.colSep,
      Path.of(csvParams.path),
      namingStrategy,
    )

    val typeSource: TypeSource = new TypeSource(csvDataSource)

    val thermalSource: ThermalSource = new ThermalSource(
      typeSource,
      csvDataSource,
    )

    val rawGridSource =
      new RawGridSource(
        typeSource,
        csvDataSource,
      )

    val energyManagementSource: EnergyManagementSource =
      new EnergyManagementSource(
        typeSource,
        csvDataSource,
      )

    val systemParticipantSource = new SystemParticipantSource(
      typeSource,
      thermalSource,
      rawGridSource,
      energyManagementSource,
      csvDataSource,
    )
    val evs = systemParticipantSource.getEvs
    if (evs.isEmpty) {
      throw new IOException(s"No evs parsed at ${csvParams.path}!")
    }
    evs.asScala.toSeq
  }

  private def getProjectRootDir: String = {
    System.getProperty("user.dir")
  }

  private def getAbsolutePath(folderPath: String): Path = {
    val path = Paths.get(folderPath)
    if (!path.isAbsolute) Paths.get(getProjectRootDir, path.toString)
    else path
  }

  def readCaseClassSeq[T](implicit
      decoder: Map[String, String] => T,
      folderPath: String,
      csvSep: String,
  ): Seq[T] = {
    val absolutePath: Path = getAbsolutePath(folderPath)

    val reader = new BufferedReader(new FileReader(absolutePath.toFile))
    val headline = reader.readLine.split(csvSep).zipWithIndex

    val lines = reader.lines.toList.asScala.toSeq
    reader.close()

    lines
      .map { row =>
        val cells = row.split(csvSep)

        headline.map { case (field, index) =>
          (field, cells(index))
        }.toMap
      }
      .map(decoder)
  }

}
