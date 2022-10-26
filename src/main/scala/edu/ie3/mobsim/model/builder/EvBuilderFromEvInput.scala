/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.model.ElectricVehicle.{
  buildEvWithType,
  determineHomePoiPdf
}
import edu.ie3.mobsim.model.{ChargingStation, ElectricVehicle, EvType}

import java.time.ZonedDateTime

object EvBuilderFromEvInput extends LazyLogging {

  def build(
      electricVehicles: Seq[EvInput],
      homePOIsWithSizes: Map[PointOfInterest, Double],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      chargingStations: Seq[ChargingStation],
      startTime: ZonedDateTime,
      targetSharePrivateCharging: Double,
      firstDepartureOfDay: FirstDepartureOfDay
  ): Set[ElectricVehicle] = {
    val (homePoiPdfWithHomeCharging, homePoiPdfWithoutHomeCharging) =
      determineHomePoiPdf(homePOIsWithSizes, chargingStations)

    /* Assign one car per home POI with home charging option */
    val amountOfHomeChargingCars =
      math.round(targetSharePrivateCharging * electricVehicles.size).intValue

    val (initialHomeChargingCars, unassignedEvs) =
      assignInitialHomeChargingCars(
        electricVehicles,
        amountOfHomeChargingCars,
        homePoiPdfWithHomeCharging,
        workPoiPdf,
        firstDepartureOfDay,
        startTime
      )

    /* Build the remaining cars */
    val additionalCars = assignRemainingCars(
      amountOfHomeChargingCars - initialHomeChargingCars.size,
      unassignedEvs,
      homePoiPdfWithHomeCharging,
      homePoiPdfWithoutHomeCharging,
      workPoiPdf,
      firstDepartureOfDay,
      startTime
    )

    require(
      initialHomeChargingCars.size + additionalCars.size == electricVehicles.size
    )

    val evs = initialHomeChargingCars.toSet ++ additionalCars
    logger.info(s"Created ${evs.size} EVs from EvInputs during setup.")
    evs
  }

  def assignInitialHomeChargingCars(
      evs: Seq[EvInput],
      amountOfHomeChargingCars: Int,
      homePoiPdfWithHomeCharging: ProbabilityDensityFunction[PointOfInterest],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      firstDepartureOfDay: FirstDepartureOfDay,
      simulationStart: ZonedDateTime
  ): (Iterable[ElectricVehicle], Seq[EvInput]) = {
    val assignedEvs = homePoiPdfWithHomeCharging.pdf.keys
      .zip(evs)
      .zipWithIndex
      .filter(_._2 < amountOfHomeChargingCars)
      .map { case (poiWithEv, idx) =>
        val (homePoi, ev) = poiWithEv
        buildEvWithType(
          s"EV_$idx",
          EvType(ev.getType),
          workPoiPdf,
          firstDepartureOfDay,
          simulationStart,
          homePoi,
          isHomeChargingPossible = true
        )
      }
    (assignedEvs, evs.drop(assignedEvs.size))
  }

  def assignRemainingCars(
      amountOfUnassignedHomeChargingCars: Int,
      unassignedEvs: Seq[EvInput],
      homePoiPdfWithHomeCharging: ProbabilityDensityFunction[PointOfInterest],
      homePoiPdfWithoutHomeCharging: ProbabilityDensityFunction[
        PointOfInterest
      ],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      firstDepartureOfDay: FirstDepartureOfDay,
      simulationStart: ZonedDateTime
  ): Seq[ElectricVehicle] = {
    unassignedEvs.zipWithIndex.map { case (ev, idx) =>
      /* As long as there are still cars unassigned with home charging option, do that, otherwise assign the rest to the
       * other home POIs */

      val (homePoi, isHomeChargingPossible) =
        if (idx < amountOfUnassignedHomeChargingCars)
          (homePoiPdfWithHomeCharging.sample(), true)
        else
          (homePoiPdfWithoutHomeCharging.sample(), false)

      buildEvWithType(
        s"EV_$idx",
        EvType(ev.getType),
        workPoiPdf,
        firstDepartureOfDay,
        simulationStart,
        homePoi,
        isHomeChargingPossible
      )
    }
  }
}
