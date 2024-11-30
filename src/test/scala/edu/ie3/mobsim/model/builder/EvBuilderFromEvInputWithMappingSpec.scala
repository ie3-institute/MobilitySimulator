/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import edu.ie3.mobsim.io.geodata.HomePoiMapping
import edu.ie3.mobsim.model.TripSimulationTestData
import edu.ie3.test.common.UnitSpec

import java.util.UUID

class EvBuilderFromEvInputWithMappingSpec
    extends UnitSpec
    with TripSimulationTestData {

  "Building evs" should {

    "work with EvInputs and maps evcs correctly" in {

      val (models, mapping) = Range(0, 5).map { _ =>
        val homePois = givenHomePoi.copy(
          uuid = UUID.randomUUID()
        )
        val evs =
          Range(0, 2).map(_ => evInput.copy().uuid(UUID.randomUUID()).build())
        val evcs = givenChargingStation.copy(uuid = UUID.randomUUID())
        val mapping =
          HomePoiMapping(homePois.uuid, evcs.uuid, evs.map(_.getUuid))
        ((homePois, evs, evcs), mapping)
      }.unzip
      val (pois, evs, evcs) = models.unzip3
      val (ev2poi, poi2evcs) = HomePoiMapping.getMaps(mapping)

      val nonMappedEv = evInput.copy().uuid(UUID.randomUUID()).build()
      val nonMappedPoi = givenHomePoi

      val builtEvs = EvBuilderFromEvInputWithEvcsMapping.build(
        evs.flatten :+ nonMappedEv,
        pois.map(poi => poi -> 1d).toMap + (nonMappedPoi -> 1d),
        givenWorkPoiPdf,
        evcs,
        givenSimulationStart,
        givenFirstDepartureMetaData,
        ev2poi,
        poi2evcs,
      )

      builtEvs.map(ev => {
        if (ev.uuid != nonMappedEv.getUuid) {
          ev.homePoi.uuid shouldBe ev2poi.getOrElse(
            ev.uuid,
            fail("Can not find UUID."),
          )
          ev.chargingAtHomePossible shouldBe true
          val nearestCs = ev.homePoi.nearestChargingStations.keys
          nearestCs.size shouldBe 1
          val cs =
            nearestCs.headOption.getOrElse(fail("No assigned charging station"))
          cs.uuid shouldBe poi2evcs(ev.homePoi.uuid)
        } else {
          ev.uuid shouldBe nonMappedEv.getUuid
          ev.chargingAtHomePossible shouldBe false
          ev.homePoi.uuid shouldBe nonMappedPoi.uuid
          ev.homePoi.nearestChargingStations.isEmpty shouldBe true
        }
      })
    }
  }
}
