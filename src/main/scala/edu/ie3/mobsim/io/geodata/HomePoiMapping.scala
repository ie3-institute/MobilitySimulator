/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.utils.IoUtils
import kantan.csv.DecodeError.TypeError
import kantan.csv.{RowDecoder, _}

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

  implicit val uuids: CellDecoder[Seq[UUID]] = CellDecoder.from((s: String) => {
    val triedUuids = s
      .split(" ")
      .filter(_.nonEmpty)
      .map(uuid => Try(UUID.fromString(uuid)))
      .toSeq
    triedUuids.partitionMap(_.toEither) match {
      case (Nil, uuids) => Right(uuids)
      case (errors, _) =>
        Left(
          TypeError(
            "Can not convert String(s) to UUID",
            errors.headOption.getOrElse(
              throw new IllegalStateException("Expected a throwable.")
            ),
          )
        )
    }
  })

  implicit val homePoiDecoder: RowDecoder[HomePoiMapping] =
    RowDecoder.decoder(0, 1, 2)(HomePoiMapping.apply)

  def readPoiMapping(csvParams: CsvParams): Seq[HomePoiMapping] = {
    IoUtils.readCaseClassSeq(
      homePoiDecoder,
      csvParams.path,
      csvParams.colSep.charAt(0),
    )
  }

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
