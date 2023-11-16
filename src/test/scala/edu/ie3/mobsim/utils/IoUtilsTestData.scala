/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.model.{ChargingBehaviorTestData, ElectricVehicle}

import java.io.File
import java.nio.file.Files
import java.time.ZonedDateTime
import java.util.UUID

trait IoUtilsTestData extends ChargingBehaviorTestData {
  protected val outputFileDir: File =
    Files.createTempDirectory("io-utils-test").toFile

  protected val ioUtils: IoUtils = IoUtils(
    outputFileDir.getPath,
    "movements.csv",
    "evs.csv",
    "evcs.csv",
    "positions.csv",
    "pois.csv",
    writeMovements = true
  )

  protected val currentTime: ZonedDateTime = ZonedDateTime.now()
  protected val status: String = "departure"
  protected val uuid: UUID = UUID.randomUUID()
  protected val firstEv: ElectricVehicle = ev1
  protected val secondEv: ElectricVehicle = ev2

  protected val evs: Seq[ElectricVehicle] = Seq(firstEv, secondEv)

  protected val poiMap
      : Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]] = {
    val poiSet: Set[PointOfInterest] = Set(charging_hub_townPoi)

    poiSet.groupBy(_.categoricalLocation).map { case (catLoc, poi) =>
      catLoc -> Set.from(poi)
    }
  }
}
