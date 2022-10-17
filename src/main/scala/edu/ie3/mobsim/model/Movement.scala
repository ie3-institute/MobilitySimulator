/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.simona.api.data.ev.model.EvModel

import java.util.UUID
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava}

/** Class to describe a movement at a charging station. If it is a deparutre or
  * arrival is given by the context
  *
  * @param cs
  *   Unique identifier of the charging station
  * @param ev
  *   Ev model
  */
final case class Movement(cs: UUID, ev: ElectricVehicle)

object Movement {

  def buildMovementsMap(
      movements: Seq[Movement]
  ): java.util.Map[UUID, java.util.List[EvModel]] = {
    movements
      .groupBy(_.cs)
      .map { case (cs, movements) =>
        (cs, movements.map(_.ev.asInstanceOf[EvModel]).asJava)
      }
      .asJava
  }

  def buildMovementsUuidMap(
      movements: Seq[Movement]
  ): java.util.Map[UUID, java.util.List[UUID]] = {
    movements
      .groupBy(_.cs)
      .map { case (cs, movements) => (cs, movements.map(_.ev.uuid).asJava) }
      .asJava
  }
}
