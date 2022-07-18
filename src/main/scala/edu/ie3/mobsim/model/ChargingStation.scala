/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.source.{RawGridSource, SystemParticipantSource}
import edu.ie3.datamodel.models.input.NodeInput
import edu.ie3.datamodel.models.input.system.EvcsInput
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import org.locationtech.jts.geom.Coordinate

import java.util.UUID
import scala.jdk.CollectionConverters._

case class ChargingStation(
    private val uuid: UUID,
    private val id: String,
    private val geoPosition: Coordinate,
    private val evcsType: ChargingPointType,
    private val evcsLocationType: EvcsLocationType,
    private val chargingPoints: Int,
    private val homeChargingStationAssignedToPOI: Boolean
) {

  def getUuid: UUID = uuid
  def getGeoPosition: Coordinate = geoPosition
  def getChargingPoints: Int = chargingPoints
  def getEvcsType: ChargingPointType = evcsType
  def getEvcsLocationType: EvcsLocationType = evcsLocationType
  def isHomeChargingStationAssignedToPOI: Boolean =
    homeChargingStationAssignedToPOI

}

case object ChargingStation extends LazyLogging {

  /** Create a list of charging stations to be used by the mobility simulator.
    * Charging stations have information on their UUID and their geographical
    * position. Uses PSDM classes and functions.
    *
    * @param evcsInput
    *   EvcsInput objects (same as in SIMONA)
    * @param nodeInput
    *   NodeInput objects (same as in SIMONA)
    * @return
    *   List of charging stations
    */
  def getChargingStationsWithPSDM(
      evcsInput: Set[EvcsInput],
      nodeInput: Set[NodeInput]
  ): Set[ChargingStation] = {

    var chargingStations: Set[ChargingStation] = Set()

    evcsInput.foreach(evcs => {
      nodeInput.foreach(node => {

        if (node.equals(evcs.getNode)) {
          val cs = new ChargingStation(
            evcs.getUuid,
            evcs.getId,
            node.getGeoPosition.getCoordinate,
            evcsType = evcs.getType,
            evcs.getLocationType,
            chargingPoints = evcs.getChargingPoints,
            false
          )
          chargingStations += cs
        }

      })
    })

    chargingStations
  }

  /** Load node and evcs input data using PSDM functions and use it to construct
    * charging stations with needed information. The information contains among
    * others the uuid (same as in SIMONA) and the geographical location
    * (coordinates).
    * @param gridSource
    *   Source to obtain grid information from
    * @param participantSource
    *   Source to obtain participant information from
    * @return
    *   A collection of available [[ChargingStation]]s
    */
  def loadChargingStationsWithPSDM(
      gridSource: RawGridSource,
      participantSource: SystemParticipantSource
  ): Set[ChargingStation] = {

    val nodeInput: Set[NodeInput] = gridSource.getNodes().asScala.toSet
    val evcsInput: Set[EvcsInput] = participantSource.getEvCS().asScala.toSet

    val chargingStations =
      ChargingStation.getChargingStationsWithPSDM(evcsInput, nodeInput)

    logger.info(
      s"Received ${chargingStations.size} charging stations during setup."
    )
    chargingStations
  }

}
