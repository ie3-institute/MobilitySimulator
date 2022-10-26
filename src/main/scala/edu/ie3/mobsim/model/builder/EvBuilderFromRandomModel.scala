/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import com.typesafe.scalalogging.LazyLogging
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

object EvBuilderFromRandomModel extends LazyLogging {

  /** Initially create all EV objects for the simulation. The EV objects are
    * saved in a mutable list. For the parametrization of the EVs, the loaded
    * probabilities are used.
    */
  def build(
      numberOfEvsInArea: Int,
      homePOIsWithSizes: Map[PointOfInterest, Double],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      chargingStations: Seq[ChargingStation],
      startTime: ZonedDateTime,
      targetSharePrivateCharging: Double,
      evModelPdf: ProbabilityDensityFunction[EvType],
      firstDepartureOfDay: FirstDepartureOfDay
  ): Set[ElectricVehicle] = {
    val (homePoiPdfWithHomeCharging, homePoiPdfWithoutHomeCharging) =
      determineHomePoiPdf(homePOIsWithSizes, chargingStations)

    /* Assign one car per home POI with home charging option */
    val amountOfHomeChargingCars =
      math.round(targetSharePrivateCharging * numberOfEvsInArea).intValue
    val initialHomeChargingCars = assignInitialHomeChargingCars(
      amountOfHomeChargingCars,
      homePoiPdfWithHomeCharging,
      workPoiPdf,
      evModelPdf,
      firstDepartureOfDay,
      startTime
    )

    /* Build the remaining cars */
    val additionalCars = assignRemainingCars(
      numberOfEvsInArea,
      amountOfHomeChargingCars,
      initialHomeChargingCars.size,
      homePoiPdfWithHomeCharging,
      homePoiPdfWithoutHomeCharging,
      workPoiPdf,
      evModelPdf,
      firstDepartureOfDay,
      startTime
    )

    val evs = initialHomeChargingCars.toSet ++ additionalCars
    logger.info(s"Created ${evs.size} EVs by model sampling during setup.")
    evs
  }

  /** Assign the first electric vehicles to home charging stations with home
    * charging possibility. The amount of cars, that are created here, is
    * limited by either the amount of available charging stations with home
    * charging option or the targeted amount of cars, that charge at home -
    * which of both applies earlier.
    *
    * @param amountOfHomeChargingCars
    *   Targeted amount of cars charging at home
    * @param homePoiPdfWithHomeCharging
    *   Probability density function for home POIs with home charging option
    * @param workPoiPdf
    *   Probability density function for work POI
    * @param evModelPdf
    *   Probability density function for ev model
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param simulationStart
    *   Wall clock time of the simulation start
    * @return
    *   A collection of electric vehicles assigned to home chargers
    */
  def assignInitialHomeChargingCars(
      amountOfHomeChargingCars: Int,
      homePoiPdfWithHomeCharging: ProbabilityDensityFunction[PointOfInterest],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      evModelPdf: ProbabilityDensityFunction[EvType],
      firstDepartureOfDay: FirstDepartureOfDay,
      simulationStart: ZonedDateTime
  ): Iterable[ElectricVehicle] = {
    homePoiPdfWithHomeCharging.pdf.keys.zipWithIndex
      .filter(
        _._2 < amountOfHomeChargingCars
      ) // Limit to the amount of home charging cars, if needed
      .map { case (homePoi, idx) =>
        buildEvWithRandomAttributes(
          s"EV_$idx",
          evModelPdf,
          workPoiPdf,
          firstDepartureOfDay,
          simulationStart,
          homePoi,
          isHomeChargingPossible = true
        )
      }
  }

  /** Create and assign the remaining amount of cars. As long as the targeted
    * amount of home-charging cars is not meat, randomly assign cars to home POI
    * with home charging option (where already one cars is assigned). If all
    * home charging cars are assigned, randomly assign cars to the home POI,
    * where no home charging is possible.
    *
    * @param amountOfEvsInArea
    *   Targeted amount of evs in area
    * @param amountOfHomeChargingCars
    *   Targeted amount of home charging evs
    * @param amountOfAssignedCars
    *   Amount of already assigned evs with home charging option
    * @param homePoiPdfWithHomeCharging
    *   Probability density function for home POI with home charging option
    * @param homePoiPdfWithoutHomeCharging
    *   Probability density function for home POI without home charging option
    * @param workPoiPdf
    *   Probability density function for work POI
    * @param evModelPdf
    *   Probability density function for ev models
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param simulationStart
    *   Wall clock time of the simulation start
    * @return
    *   A collection of evs with and without home charging
    */
  def assignRemainingCars(
      amountOfEvsInArea: Int,
      amountOfHomeChargingCars: Int,
      amountOfAssignedCars: Int,
      homePoiPdfWithHomeCharging: ProbabilityDensityFunction[PointOfInterest],
      homePoiPdfWithoutHomeCharging: ProbabilityDensityFunction[
        PointOfInterest
      ],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      evModelPdf: ProbabilityDensityFunction[EvType],
      firstDepartureOfDay: FirstDepartureOfDay,
      simulationStart: ZonedDateTime
  ): Seq[ElectricVehicle] = {
    val (amountOfUnassignedHomeChargingCars, amountOfUnassignedCars) =
      determineUnassignedCars(
        amountOfEvsInArea,
        amountOfHomeChargingCars,
        amountOfAssignedCars
      )
    Range(0, amountOfUnassignedCars).map { cnt =>
      /* As long as there are still cars unassigned with home charging option, do that, otherwise assign the rest to the
       * other home POIs */
      val (homePoi, isHomeChargingPossible) =
        if (cnt < amountOfUnassignedHomeChargingCars)
          (homePoiPdfWithHomeCharging.sample(), true)
        else
          (homePoiPdfWithoutHomeCharging.sample(), false)
      val idx = cnt + amountOfAssignedCars

      buildEvWithRandomAttributes(
        s"EV_$idx",
        evModelPdf,
        workPoiPdf,
        firstDepartureOfDay,
        simulationStart,
        homePoi,
        isHomeChargingPossible
      )
    }
  }

  /** Determine the overall amount of unassigned cars as well as the amount of
    * unassigned cars, that are meant to charge at home
    *
    * @param amountOfEvsInArea
    *   Targeted amount of evs
    * @param amountOfHomeChargingCars
    *   Targeted amount of evs, that charge at home
    * @param amountOfAssignedCars
    *   Amount of already assigned cars, that charge at home
    * @return
    *   The amount of unassigned cars, that charge at home and the overall
    *   amount
    */
  private def determineUnassignedCars(
      amountOfEvsInArea: Int,
      amountOfHomeChargingCars: Int,
      amountOfAssignedCars: Int
  ): (Int, Int) =
    (
      math.max(amountOfHomeChargingCars - amountOfAssignedCars, 0),
      math.max(amountOfEvsInArea - amountOfAssignedCars, 0)
    )

  /** Build electric vehicle model with the following random attributes: Model,
    * work POI and first departure of day
    *
    * @param id
    *   Human-readable identifier
    * @param evModelPdf
    *   Probability density function for the model
    * @param workPoiPdf
    *   Probability density function for the work POI
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param startTime
    *   Wall clock time of the simulation start
    * @param homePoi
    *   Known home POI of the car
    * @param isHomeChargingPossible
    *   Whether or not charging at home is possible
    * @return
    *   An electric vehicle model
    */
  private def buildEvWithRandomAttributes(
      id: String,
      evModelPdf: ProbabilityDensityFunction[EvType],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      firstDepartureOfDay: FirstDepartureOfDay,
      startTime: ZonedDateTime,
      homePoi: PointOfInterest,
      isHomeChargingPossible: Boolean
  ): ElectricVehicle = {
    /* Sample the ev model */
    val evType = evModelPdf.sample()
    buildEvWithType(
      id,
      evType,
      workPoiPdf,
      firstDepartureOfDay,
      startTime,
      homePoi,
      isHomeChargingPossible
    )

  }
}
