/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.datamodel.io.csv.BufferedCsvWriter
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.model.{ChargingStation, ElectricVehicle}
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOWATT,
  KILOWATTHOUR,
  KILOWATTHOUR_PER_KILOMETRE
}

import java.io.File
import java.nio.file.{Files, Paths}
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
      "time" -> currentTime.toString,
      "ev" -> ev.getUuid.toString,
      "status" -> status,
      "soc" -> (ev.getStoredEnergy.getValue.doubleValue / ev.getEStorage.getValue.doubleValue).toString,
      "destination_poi" -> ev.getDestinationPoi.id,
      "categorical_location" -> ev.getDestinationCategoricalLocation.toString,
      "scheduled_departure" -> ev.getDepartureTime.toString,
      "is_charging" -> ev.isChargingAtSimona.toString
    ).asJava

    movementWriter.write(fieldData)
  }

  /** Write ev details to csv file
    *
    * @param electricVehicles
    *   Collection of electric vehicles
    */
  def writeEvs(
      electricVehicles: Set[ElectricVehicle]
  ): Unit = electricVehicles.foreach { ev =>
    val fieldData = Map(
      "uuid" -> ev.getUuid.toString,
      "id" -> ev.getId,
      "model" -> ev.getModel,
      "battery_capacity" ->
        ev.getEStorage.to(KILOWATTHOUR).getValue.doubleValue().toString,
      "max_charging_power_ac" ->
        ev.getSRatedAC.to(KILOWATT).getValue.doubleValue().toString,
      "max_charging_power_dc" ->
        ev.getSRatedDC.to(KILOWATT).getValue.doubleValue().toString,
      "consumption" ->
        ev.getConsumption
          .to(KILOWATTHOUR_PER_KILOMETRE)
          .getValue
          .doubleValue()
          .toString,
      "home_poi" -> ev.getHomePOI.id,
      "work_poi" -> ev.getWorkPOI.id,
      "is_home_charging_possible" -> ev.isChargingAtHomePossible.toString
    ).asJava

    evWriter.write(fieldData)
  }

  /** Define relevant data for charging station and call IO function to write it
    * to csv.
    *
    * @param cs
    *   charging station
    * @param availableChargingPoints
    *   current mapping of available charging points
    * @param currentTime
    *   current time
    * @param uuid
    *   Unique identifier fot the entry
    */
  def writeEvcs(
      cs: ChargingStation,
      availableChargingPoints: Map[UUID, Int],
      currentTime: ZonedDateTime,
      uuid: UUID = UUID.randomUUID()
  ): Unit = {
    val fieldData = Map(
      "uuid" -> uuid.toString,
      "time" -> currentTime.toString,
      "evcs" -> cs.getUuid.toString,
      "charging_points" -> cs.getChargingPoints.toString,
      "occupied_charging_points" -> (cs.getChargingPoints - availableChargingPoints
        .getOrElse(cs.getUuid, 0)
        .asInstanceOf[Int]).toString
    ).asJava

    evcsWriter.write(fieldData)
  }

  /** Write given POIs to csv file
    *
    * @param pois
    *   POIs to write
    */
  def writePois(
      pois: Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]]
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
      if (time.isBefore(ev.getParkingTimeStart)) {
        ("DRIVING", "")
      } else {
        (
          ev.getDestinationCategoricalLocation.toString,
          ev.getDestinationPoi.toString
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
          "time",
          "ev",
          "status",
          "soc",
          "destination_poi",
          "categorical_location",
          "scheduled_departure",
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
          "occupied_charging_points"
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
}
