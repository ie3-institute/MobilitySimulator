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
import java.time.ZonedDateTime
import java.util.UUID
import scala.collection.immutable.TreeSet

trait IoUtilsTestData extends ChargingBehaviorTestData {
  protected val outputFileFolder: String = new java.io.File(
    "."
  ).getCanonicalPath + File.separator + "out" + File.separator

  protected val ioUtils: IoUtils = IoUtils(
    outputFileFolder,
    "movements",
    "evs",
    "evcs",
    "positions",
    "pois"
  )

  protected val currentTime: ZonedDateTime = ZonedDateTime.now()
  protected val status: String = "departure"
  protected val uuid: UUID = UUID.randomUUID()
  protected val firstEv: ElectricVehicle = ev1
  protected val secondEv: ElectricVehicle = ev2

  protected val evSet: Set[ElectricVehicle] = Seq(firstEv, secondEv).toSet

  protected val poiMap
      : Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]] = {
    val poiSet: Set[PointOfInterest] = Seq(charging_hub_townPoi).toSet

    poiSet.groupBy(_.categoricalLocation).map { case (catLoc, poi) =>
      catLoc -> TreeSet.from(poi)
    }
  }
}
