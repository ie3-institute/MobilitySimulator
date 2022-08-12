/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.source.{RawGridSource, SystemParticipantSource}
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import org.locationtech.jts.geom.Coordinate

import java.util.UUID
import scala.jdk.CollectionConverters._

case class ChargingStation(
    uuid: UUID,
    id: String,
    geoPosition: Coordinate,
    evcsType: ChargingPointType,
    evcsLocationType: EvcsLocationType,
    chargingPoints: Int
)

object ChargingStation extends LazyLogging {

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
  ): Seq[ChargingStation] = {

    val evcsInput = participantSource.getEvCS().asScala.toSeq

    val chargingStations = evcsInput.map { evcs =>
      ChargingStation(
        evcs.getUuid,
        evcs.getId,
        evcs.getNode.getGeoPosition.getCoordinate,
        evcsType = evcs.getType,
        evcs.getLocationType,
        chargingPoints = evcs.getChargingPoints
      )
    }

    logger.info(
      s"Received ${chargingStations.size} charging stations during setup."
    )
    chargingStations
  }

}
