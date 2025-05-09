/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.exceptions.SourceException
import edu.ie3.mobsim.utils.IoUtils

import java.util.UUID
import scala.util.Try

final case class HomePoiMapping(
    poi: UUID,
    evcs: UUID,
    evs: Seq[UUID],
)

object HomePoiMapping {

  val header: Seq[String] = Seq("poi", "evcs", "evs")

  def evString(evs: Seq[UUID]): String = {
    evs.foldLeft("")((str, uuid) => str + uuid.toString + " ").strip()
  }

  implicit val uuids: String => Seq[UUID] = (s: String) => {
    val triedUuids = s.split(" ").map(uuid => Try(UUID.fromString(uuid))).toSeq
    triedUuids.partitionMap(_.toEither) match {
      case (Nil, uuids) => uuids
      case (errors, _) =>
        throw SourceException("Can not convert String(s) to UUID")
    }
  }

  implicit val homePoiDecoder: Map[String, String] => HomePoiMapping =
    row =>
      HomePoiMapping(
        UUID.fromString(row("poi")),
        UUID.fromString(row("evcs")),
        uuids(row("evs")),
      )

  def readPoiMapping(csvParams: CsvParams): Seq[HomePoiMapping] =
    IoUtils.readCaseClassSeq(
      homePoiDecoder,
      csvParams.path,
      csvParams.colSep,
    )

  def getMaps(
      mappingEntries: Seq[HomePoiMapping]
  ): (Map[UUID, UUID], Map[UUID, UUID]) = {
    val ev2poi = mappingEntries
      .flatMap(entry => entry.evs.map(_ -> entry.poi))
      .toMap
    val poi2evcs =
      mappingEntries.map(entry => entry.poi -> entry.evcs).toMap
    (ev2poi, poi2evcs)
  }
}
