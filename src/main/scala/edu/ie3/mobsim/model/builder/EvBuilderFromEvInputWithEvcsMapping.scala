/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.model.{ChargingStation, ElectricVehicle, EvType}
import edu.ie3.util.geo.GeoUtils

import java.time.ZonedDateTime
import java.util.UUID

object EvBuilderFromEvInputWithEvcsMapping {

  def build(
      electricVehicles: Seq[EvInput],
      homePOIsWithSizes: Map[PointOfInterest, Double],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      chargingStations: Seq[ChargingStation],
      startTime: ZonedDateTime,
      firstDepartureOfDay: FirstDepartureOfDay,
      ev2homePoi: Map[UUID, UUID],
      homePoi2evcs: Map[UUID, UUID]
  ): Set[ElectricVehicle] = {
    val homePoiMap = homePOIsWithSizes.keys.map(poi => poi.uuid -> poi).toMap
    val evcsMap = chargingStations.map(evcs => evcs.uuid -> evcs).toMap
    val evMap = electricVehicles.map(ev => ev.getUuid -> ev).toMap

    // assign evcs to home poi from mapping
    val homePoisWithEvcsMap = assignEvcsToHomePoi(
      homePoi2evcs,
      homePoiMap,
      evcsMap
    )

    val evType2homeChargingEv = ElectricVehicle.buildEvWithType(
      _,
      _,
      _,
      workPoiPdf,
      firstDepartureOfDay,
      startTime,
      _,
      _
    )

    // assign home ev to corresponding home poi
    val homeChargingCars = assignEvToHomePoiWithMapping(
      ev2homePoi,
      homePoisWithEvcsMap,
      evMap,
      evType2homeChargingEv
    )

    // assign rest of evs to home poi that is not inside mapping
    val homeChargingPoisUuids = homePoi2evcs.keys.toSet
    val (_, homePoisWithoutCharging) =
      homePOIsWithSizes.partition { case (poi, _) =>
        homeChargingPoisUuids.contains(poi.uuid)
      }

    val homeChargingCarsUuidSet = ev2homePoi.keys.toSet
    val homePoiPdfWithoutHomeCharging = ProbabilityDensityFunction(
      homePoisWithoutCharging
    )

    // every ev that does not have a mapped evcs does not have the option to charge at home
    val nonHomeChargingCars = electricVehicles
      .filter(ev => !homeChargingCarsUuidSet.contains(ev.getUuid))
      .zipWithIndex
      .map { case (ev, idx) =>
        val homePoi = homePoiPdfWithoutHomeCharging.sample()
        evType2homeChargingEv(
          s"EV_$idx",
          ev.getUuid,
          EvType(ev.getType),
          homePoi,
          false
        )
      }
    (homeChargingCars ++ nonHomeChargingCars).toSet
  }

  private def assignEvcsToHomePoi(
      homePoi2EvcsUuid: Map[UUID, UUID],
      homePoiMap: Map[UUID, PointOfInterest],
      evcsMap: Map[UUID, ChargingStation]
  ): Map[UUID, PointOfInterest] = {
    homePoi2EvcsUuid.map { case (homePoiUuid, evcsUuid) =>
      val evcs = evcsMap.getOrElse(
        evcsUuid,
        throw new IllegalArgumentException(
          s"Evcs with UUID: $evcsUuid could not be found"
        )
      )
      val homePoi = homePoiMap.getOrElse(
        homePoiUuid,
        throw new IllegalArgumentException(
          s"Home poi with UUID: $evcsUuid could not be found"
        )
      )
      val poiCoordinate = homePoi.geoPosition
      val distance = GeoUtils.calcHaversine(
        poiCoordinate.y,
        poiCoordinate.x,
        evcs.geoPosition.y,
        evcs.geoPosition.x
      )
      homePoi.uuid -> homePoi.copy(nearestChargingStations =
        Map(evcs -> distance)
      )
    }

  }

  private def assignEvToHomePoiWithMapping(
      ev2homePoiUuid: Map[UUID, UUID],
      homePoiMap: Map[UUID, PointOfInterest],
      evMap: Map[UUID, EvInput],
      evType2ev: (
          String,
          UUID,
          EvType,
          PointOfInterest,
          Boolean
      ) => ElectricVehicle
  ): Seq[ElectricVehicle] = {
    ev2homePoiUuid.zipWithIndex.map { case ((evUuid, poiUuid), idx) =>
      val ev = evMap.getOrElse(
        evUuid,
        throw new IllegalArgumentException(
          s"Ev with UUID: $evUuid could not be found"
        )
      )
      val homePoi = homePoiMap.getOrElse(
        poiUuid,
        throw new IllegalArgumentException(
          s"Home poi with UUID: $poiUuid could not be found"
        )
      )
      evType2ev(s"EV_$idx", ev.getUuid, EvType(ev.getType), homePoi, true)
    }.toSeq
  }
}
