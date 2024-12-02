/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.mobsim.MobilitySimulator.seed
import edu.ie3.mobsim.exceptions.TripException
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary.{HOME, WORK}
import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary,
}
import edu.ie3.mobsim.io.geodata.{PoiEnums, PointOfInterest}
import edu.ie3.mobsim.io.probabilities._
import edu.ie3.mobsim.utils.DefaultQuantities._
import edu.ie3.mobsim.utils.sq.SpecificEnergyDistance
import edu.ie3.mobsim.utils.sq.SquantsUtils.{RichDistance, RichEnergy}
import edu.ie3.mobsim.utils.{IoUtils, utils}
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOMETRE,
  KILOWATT,
  KILOWATTHOUR,
}
import squants.{Energy, Length, Time}
import squants.energy.{KilowattHours, WattHours}
import squants.motion.Velocity
import squants.space.Kilometers
import squants.time.{Hours, Minutes}
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import scala.annotation.tailrec
import scala.math.Ordering.Implicits.infixOrderingOps

object TripSimulation extends LazyLogging {

  /** Simulate the next trip for an EV object. <p> Sample and calculate new
    * values such as destination POI, departure time, and SOC.
    *
    * @param currentTime
    *   current time
    * @param ev
    *   EV object for which a new trip shall be generated
    * @param poisWithSizes
    *   POIs in the area with size information
    * @param chargingHubTownIsPresent
    *   boolean if charging hub town is present in area
    * @param chargingHubHighwayIsPresent
    *   boolean if charging hub town is present in area
    * @param ioUtils
    *   Utilities to write results to csv
    * @param tripProbabilities
    *   Probabilities to generate new trips
    * @param thresholdChargingHubDistance
    *   Maximum permissible distance to the next charging hub
    * @return
    *   The updated EV
    */
  def simulateNextTrip(
      currentTime: ZonedDateTime,
      ev: ElectricVehicle,
      poisWithSizes: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest],
      ],
      chargingHubTownIsPresent: Boolean,
      chargingHubHighwayIsPresent: Boolean,
      chargingStations: Seq[ChargingStation],
      ioUtils: IoUtils,
      tripProbabilities: TripProbabilities,
      thresholdChargingHubDistance: squants.Length,
      round15: Boolean,
  ): ElectricVehicle = {

    /* Save EV movement to csv before trip */
    if (ioUtils.writeMovements)
      ioUtils.writeMovement(ev, currentTime, "departure")

    val socAtStartOfTrip: Double =
      ev.getStoredEnergy.divide(ev.getEStorage).getValue.doubleValue()

    val categoricalLocationToPdf = poisWithSizes.map {
      case (categoricalLocation, pdf) =>
        categoricalLocation -> pdf
    }
    /* Get planned POI information and distance for the trip */
    val (
      plannedDestinationPoiType,
      plannedDestinationPoi,
      plannedDrivingDistance,
      changedEv,
    ) = getTargetProperties(
      ev,
      currentTime,
      categoricalLocationToPdf,
      tripProbabilities.categoricalLocation,
      tripProbabilities.poiTransition,
      tripProbabilities.tripDistance,
    ) match {
      case TargetProperties(
            poiType,
            poi,
            distance,
            alteredEv,
          ) =>
        (poiType, poi, distance, alteredEv)
    }

    /* Simulate the planned trip */
    val (
      plannedStoredEnergyEndOfTrip,
      plannedParkingTimeStart,
      plannedDepartureTime,
    ) = simulatePlannedTrip(
      changedEv,
      currentTime,
      plannedDrivingDistance,
      plannedDestinationPoiType,
      tripProbabilities.drivingSpeed,
      tripProbabilities.firstDepartureOfDay,
      tripProbabilities.lastTripOfDay,
      tripProbabilities.parkingTime,
      round15,
    )

    /* Decide whether EV makes a stop at a charging hub to recharge during the trip */
    val (evWantsToChargeAtChargingHub, maybeSocAtArrival) =
      doesEvWantToChargeAtChargingHub(
        changedEv,
        plannedStoredEnergyEndOfTrip,
        plannedDestinationPoi.categoricalLocation,
        plannedParkingTimeStart,
        plannedDepartureTime,
      )

    maybeSocAtArrival match {
      case Some(socAtArrival) if evWantsToChargeAtChargingHub =>
        (
          plannedDrivingDistance <=
            thresholdChargingHubDistance,
          chargingHubTownIsPresent,
          chargingHubHighwayIsPresent,
        ) match {
          case (true, true, _) =>
            makeTripToChargingHub(
              PoiTypeDictionary.CHARGING_HUB_TOWN,
              changedEv,
              currentTime,
              poisWithSizes,
              socAtStartOfTrip,
              socAtArrival,
              plannedDrivingDistance,
              plannedDestinationPoi,
              plannedDestinationPoiType,
              chargingStations,
              tripProbabilities.drivingSpeed,
            )
          case (true, false, _) =>
            logger.debug(
              s"${ev.getId} wants to charge at charging hub on a <50km trip, but keeps its original trip because there are no chargingHubTown."
            )
            keepOriginalTrip(
              changedEv,
              plannedStoredEnergyEndOfTrip,
              plannedDestinationPoi,
              plannedDestinationPoiType,
              plannedParkingTimeStart,
              plannedDepartureTime,
            )
          case (false, _, true) =>
            makeTripToChargingHub(
              PoiTypeDictionary.CHARGING_HUB_HIGHWAY,
              changedEv,
              currentTime,
              poisWithSizes,
              socAtStartOfTrip,
              socAtArrival,
              plannedDrivingDistance,
              plannedDestinationPoi,
              plannedDestinationPoiType,
              chargingStations,
              tripProbabilities.drivingSpeed,
            )
          case (false, true, false) =>
            makeModifiedTripToChargingHub(
              PoiTypeDictionary.CHARGING_HUB_TOWN,
              changedEv,
              currentTime,
              poisWithSizes,
              socAtArrival,
              plannedDrivingDistance,
              plannedDestinationPoi,
              plannedDestinationPoiType,
              chargingStations,
              tripProbabilities.drivingSpeed,
            )
          case (false, false, false) =>
            logger.debug(
              s"${ev.getId} wants to charge at charging hub on a >50km trip, but keeps its original trip because there are no chargingHubs at all."
            )
            keepOriginalTrip(
              changedEv,
              plannedStoredEnergyEndOfTrip,
              plannedDestinationPoi,
              plannedDestinationPoiType,
              plannedParkingTimeStart,
              plannedDepartureTime,
            )
        }
      case _ =>
        /* Do not change the planned trip */

        keepOriginalTrip(
          changedEv,
          plannedStoredEnergyEndOfTrip,
          plannedDestinationPoi,
          plannedDestinationPoiType,
          plannedParkingTimeStart,
          plannedDepartureTime,
        )
    }

  }

  /** Get planned POI information and distance for the next trip. If the
    * previous trip was to a charging hub, use the saved information on its
    * original trip for the next trip. Otherwise, sample new information.
    *
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param currentTime
    *   current time
    * @param categoricalLocationToPdf
    *   POIs in the area with size information
    * @param categoricalLocation
    *   Meta-information to determine the next categorical location
    * @param poiTransition
    *   Meta-information to determine the next POI transition
    * @param tripDistance
    *   Meta-information to determine the distance of the next trip
    * @return
    *   POI information and planned trip distance for the trip
    */
  private def getTargetProperties(
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      // TODO: Check, if this is double information!
      categoricalLocationToPdf: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest],
      ],
      categoricalLocation: CategoricalLocation,
      poiTransition: PoiTransition,
      tripDistance: TripDistance,
  ): TargetProperties = {
    val currentPoiType = ev.destinationPoiType
    if (
      currentPoiType == PoiTypeDictionary.CHARGING_HUB_TOWN || currentPoiType == PoiTypeDictionary.CHARGING_HUB_HIGHWAY
    ) {
      /* If the ev has been at a charging hub: Resume the previous trip */
      resumeTrip(ev)
    } else {
      /* Sample a new target */
      sampleNewTarget(
        ev,
        currentTime,
        categoricalLocation,
        categoricalLocationToPdf,
        poiTransition,
        tripDistance,
      )
    }
  }

  /** Properties of a target for a trip
    * @param poiType
    *   Type of the point of interest
    * @param poi
    *   Actual target POI
    * @param distance
    *   Distance to be driven#
    * @param ev
    *   The vehicle that drives the distance to the poi. This parameter is used
    *   to return updates of the vehicle.
    */
  private final case class TargetProperties(
      poiType: PoiTypeDictionary.Value,
      poi: PointOfInterest,
      distance: Length,
      ev: ElectricVehicle,
  )

  /** If the vehicles has intermediately been charged at a charging hub, resume
    * the last trip. By doing this, the trip properties are received from the
    * vehicle, reset there (CAUTION / ATTENTION / UWAGA: This method has side
    * effects!) and given back as new trip parameters
    *
    * @param ev
    *   The vehicle to receive information from
    * @return
    *   The trip properties
    */
  private def resumeTrip(
      ev: ElectricVehicle
  ): TargetProperties =
    (
      ev.finalDestinationPoi,
      ev.finalDestinationPoiType,
      ev.remainingDistanceAfterChargingHub,
    ) match {
      case (
            Some(destinationPoi),
            Some(destinationPoiType),
            Some(remainingDistance),
          ) =>
        /* Reset saved values */
        val updatedEv: ElectricVehicle = ev
          .resetFinalDestination()
          .setRemainingDistanceAfterChargingHub(None)

        /* Return the determined values */
        TargetProperties(
          destinationPoiType,
          destinationPoi,
          Kilometers(remainingDistance.to(KILOMETRE).getValue.doubleValue()),
          updatedEv,
        )
      case (None, _, _) =>
        throw TripException(
          "Cannot resume trip, as the previous destination POI is unknown."
        )
      case (Some(_), Some(_), None) =>
        throw TripException(
          "Cannot resume trip, as the remaining distance is unknown."
        )
      case (_, None, _) =>
        throw TripException(
          "Cannot resume trip, as the final destination poi type is unknown-"
        )
    }

  /** Sample a new target for the trip
    *
    * @param ev
    *   Vehicle to make the trip
    * @param time
    *   Current wall clock time
    * @param categoricalLocation
    *   Meta-information to determine next categorical location
    * @param categoricalLocationToPdf
    *   Mapping from categorical location type to POI probabilities
    * @param poiTransition
    *   Meta-information to determine next POI transition
    * @param tripDistance
    *   Meta-information to determine next trip distance
    * @return
    *   The target properties
    */
  private def sampleNewTarget(
      ev: ElectricVehicle,
      time: ZonedDateTime,
      categoricalLocation: CategoricalLocation,
      categoricalLocationToPdf: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[
          PointOfInterest
        ],
      ],
      poiTransition: PoiTransition,
      tripDistance: TripDistance,
  ): TargetProperties = {
    /* Save previous POI type (is required for later calculations) */
    val previousPoiType = ev.destinationPoiType

    /* Sample next destination POI type */
    val destinationPoiType =
      poiTransition.sample(time, previousPoiType)

    /* Sample specific destination POI instance */
    sampleNextPoi(
      destinationPoiType,
      time,
      ev.homePoi,
      ev.workPoi,
      categoricalLocation,
      categoricalLocationToPdf,
    ) match {
      case poi =>
        /* Sample driving distance */
        val drivingDistance: Length =
          tripDistance.sample(
            time,
            previousPoiType,
            destinationPoiType,
          )
        TargetProperties(
          destinationPoiType,
          poi,
          drivingDistance,
          ev,
        )
    }
  }

  /** Sample the next actual POI based on the destination POI type. If it is
    * [[PoiTypeDictionary.HOME]] or [[PoiTypeDictionary.WORK]] the fixed
    * assigned POIs are used. If it's another type, a categorical location as
    * well as a random POI is sampled.
    *
    * @param destinationPoiType
    *   Type of POI to travel to
    * @param time
    *   Wall clock time
    * @param homePoi
    *   Fixed assigned Home POI
    * @param workPoi
    *   Fixed assigned Work POI
    * @param categoricalLocation
    *   Meta-information to determine the next categorical location
    * @return
    *   The resulting POI
    */
  private def sampleNextPoi(
      destinationPoiType: PoiTypeDictionary.Value,
      time: ZonedDateTime,
      homePoi: PointOfInterest,
      workPoi: PointOfInterest,
      categoricalLocation: CategoricalLocation,
      categoricalLocationToPdf: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[
          PointOfInterest
        ],
      ],
  ): PointOfInterest = {
    destinationPoiType match {
      case HOME =>
        /* POI type "Home" is directly mapped to a fixed home POI */
        homePoi
      case WORK =>
        /* POI type "Work" is directly mapped to a fixed work POI */
        workPoi
      case _ =>
        /* Any other POI type. Sample a random categorical location and based on that a specific POI. */
        sampleDestinationCategoricalLocationAndPoi(
          time,
          destinationPoiType,
          categoricalLocation,
          categoricalLocationToPdf,
        )
    }
  }

  /** Sample a categorical location and an equivalent actual target POI based on
    * the given information. If for the categorical location no target POI is
    * available at all, the categorical location is recursively re-sampled until
    * a POI can be found or a specified recursion depth is reached. If finally
    * no POI can be found, a [[TripException]] is thrown.
    *
    * @param time
    *   current wallclock time
    * @param destinationPoiType
    *   Type of the destination POI
    * @param categoricalLocation
    *   Meta-information to determine the next categorical location
    * @param depth
    *   Current recursion depth (defaults to 0)
    * @param maxDepth
    *   Max recursion depth (defaults to 4)
    * @return
    *   The resulting destination POI
    */
  @tailrec
  private def sampleDestinationCategoricalLocationAndPoi(
      time: ZonedDateTime,
      destinationPoiType: PoiTypeDictionary.Value,
      categoricalLocation: CategoricalLocation,
      categoricalLocationToPdf: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[
          PointOfInterest
        ],
      ],
      depth: Int = 0,
      maxDepth: Int = 4,
  ): PointOfInterest = {
    val nextCategoricalLocation =
      categoricalLocation.sample(time, destinationPoiType)
    categoricalLocationToPdf.get(nextCategoricalLocation) match {
      case Some(pdf) =>
        pdf.sample()
      case None =>
        if (depth > maxDepth) {
          throw TripException(
            s"Unable to determine possible target categorical location and poi after $maxDepth tries."
          )
        } else {
          logger.warn(
            s"There are no target POIs available for categorical location $nextCategoricalLocation. Resample categorical location."
          )
          sampleDestinationCategoricalLocationAndPoi(
            time,
            destinationPoiType,
            categoricalLocation,
            categoricalLocationToPdf,
            depth + 1,
            maxDepth,
          )
        }
    }
  }

  /** Simulate the planned trip based on the planned information on destination
    * and distance.
    *
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param currentTime
    *   current time
    * @param plannedDrivingDistance
    *   planned trip distance
    * @param plannedDestinationPoiType
    *   planned destination POI type
    * @param drivingSpeed
    *   Meta-information to sample the next driving speed
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param lastTripOfDay
    *   Meta-information to determine if that trip is the last trip of the day
    * @param parkingTime
    *   Meta-information to determine the parking time
    * @return
    *   simulated parameters for the trip
    */
  private def simulatePlannedTrip(
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      plannedDrivingDistance: Length,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      drivingSpeed: DrivingSpeed,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime,
      round15: Boolean,
  ): (Energy, ZonedDateTime, ZonedDateTime) = {

    /* Calculate stored energy at the end of the trip based on planned values */
    val plannedStoredEnergyEndOfTrip: Energy =
      calculateStoredEnergyAtEndOfTrip(
        ev.evType.consumption,
        KilowattHours(
          ev.getStoredEnergy.to(KILOWATTHOUR).getValue.doubleValue()
        ),
        plannedDrivingDistance: Length,
      )

    /* Sample driving speed based on planned values */
    val plannedDrivingSpeed = drivingSpeed
      .sample(currentTime, plannedDrivingDistance)

    /* Calculate driving time based on planned values */
    val rawDrivingTime: Time =
      (plannedDrivingDistance / plannedDrivingSpeed).max(Hours(1))

    val plannedDrivingTime: Time =
      if (round15)
        Minutes(
          utils.roundToQuarterHourInMinutes(rawDrivingTime.toMinutes.toInt)
        )
      else rawDrivingTime

    /* Calculate start of parking time based on planned values */
    val plannedParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(plannedDrivingTime.toMinutes.toLong)

    /* Calculate departure time based on planned values */
    val plannedDepartureTime: ZonedDateTime = calculateDepartureTime(
      plannedDestinationPoiType,
      plannedParkingTimeStart,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime,
    )

    (
      plannedStoredEnergyEndOfTrip,
      plannedParkingTimeStart,
      plannedDepartureTime,
    )
  }

  /** Change the planned trip and make a trip to a charging hub. Which type of
    * charging hub is defined by chargingHubPoiType.
    *
    * @param chargingHubPoiType
    *   type of charging hub to drive to
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param currentTime
    *   current time
    * @param poisWithSizes
    *   POIs in the area with size information
    * @param socAtStartOfTrip
    *   SoC at start of the trip
    * @param plannedDrivingDistance
    *   planned trip distance
    * @param plannedDestinationPoi
    *   planned destination POI
    * @param plannedDestinationPoiType
    *   planned destination POI type
    * @param drivingSpeed
    *   Meta-information to determine the next driving speed
    * @return
    *   The updated EV
    */
  def makeTripToChargingHub(
      chargingHubPoiType: PoiTypeDictionary.Value,
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      poisWithSizes: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest],
      ],
      socAtStartOfTrip: Double,
      socAtChargingHubArrival: Double,
      plannedDrivingDistance: Length,
      plannedDestinationPoi: PointOfInterest,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      chargingStations: Seq[ChargingStation],
      drivingSpeed: DrivingSpeed,
  ): ElectricVehicle = {

    /* Drive only until SoC reaches SoC of charging hub arrival */
    val newStoredEnergyEndOfTrip: Energy = KilowattHours(
      ev.getEStorage
        .to(KILOWATTHOUR)
        .getValue
        .doubleValue() * socAtChargingHubArrival
    )
    /* Reduced used energy for the trip until the charging hub */
    val usedEnergyForThisTrip: Energy = KilowattHours(
      ev.getStoredEnergy.to(KILOWATTHOUR).getValue.doubleValue()
    ) - newStoredEnergyEndOfTrip

    /* Reduced driving distance to the charging hub */
    val newDrivingDistance: Length =
      usedEnergyForThisTrip.calcDistance(ev.evType.consumption)

    /* Sample driving speed (based on original driving distance) */
    val newDrivingSpeed: Velocity =
      drivingSpeed
        .sample(
          currentTime,
          plannedDrivingDistance,
        )

    /* Calculate driving time */
    val newDrivingTime: Int = math.max(
      (math rint (newDrivingDistance / newDrivingSpeed).toMinutes).toInt,
      1,
    )

    /* Calculate start of parking time */
    val newParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(newDrivingTime)

    val newDestinationCategoricalLocation: CategoricalLocationDictionary.Value =
      PoiEnums.CategoricalLocationDictionary(chargingHubPoiType)
    /* Sample destination POI */
    val newDestinationPoi: PointOfInterest =
      poisWithSizes(
        newDestinationCategoricalLocation
      ).sample()

    /* Calculate parking time */
    val newParkingTime: Time =
      calculateChargingTimeAtChargingHub(
        ev,
        newDestinationPoi,
        newStoredEnergyEndOfTrip,
        chargingStations,
      )

    /* Calculate departure time */
    val newDepartureTime: ZonedDateTime =
      newParkingTimeStart.plusMinutes(newParkingTime.toMinutes.toLong)

    /* Save parameters of the originally planned trip for the next trip simulation after charging */
    val updatedEv: ElectricVehicle = ev
      .setRemainingDistanceAfterChargingHub(
        Some(
          Quantities.getQuantity(
            (plannedDrivingDistance - newDrivingDistance).toKilometers,
            PowerSystemUnits.KILOMETRE,
          )
        )
      )
      .setFinalDestination(plannedDestinationPoi, plannedDestinationPoiType)

    /* Create updated EV */
    updatedEv.copyWith(
      Quantities
        .getQuantity(newStoredEnergyEndOfTrip.toKilowattHours, KILOWATTHOUR),
      newDestinationPoi,
      chargingHubPoiType,
      newParkingTimeStart,
      newDepartureTime,
    )
  }

  /** Change the planned trip and make a trip to a charging hub. The trip is
    * modified, because the trip distance is > 50 km, which implicated that the
    * EV is driving on highways, but there are no highway charging hubs in the
    * area. Therefore, it is assumed that the EV leaves the considered area and
    * comes back and charges at a local charging hub in town with a fixed SoC at
    * arrival and a fixed remaining driving distance after the charging hub
    * stop.
    *
    * @param chargingHubPoiType
    *   type of charging hub to drive to
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param currentTime
    *   current time
    * @param poisWithSizes
    *   POIs in the area with size information
    * @param plannedDrivingDistance
    *   planned trip distance
    * @param plannedDestinationPoi
    *   planned destination POI
    * @param drivingSpeed
    *   Meta-information to determine the next driving speed
    * @return
    */
  def makeModifiedTripToChargingHub(
      chargingHubPoiType: PoiTypeDictionary.Value,
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      poisWithSizes: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest],
      ],
      socAtChargingHubArrival: Double,
      plannedDrivingDistance: Length,
      plannedDestinationPoi: PointOfInterest,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      chargingStations: Seq[ChargingStation],
      drivingSpeed: DrivingSpeed,
  ): ElectricVehicle = {

    /* Set SoC at arriving at charging hub in town */
    val newStoredEnergyEndOfTrip: Energy = KilowattHours(
      ev.getEStorage
        .to(KILOWATTHOUR)
        .getValue
        .doubleValue() * socAtChargingHubArrival
    )

    /* Set driving distance to the charging hub to fixed value, so that only 10 km are left afterwards */
    val newDrivingDistance: Length =
      plannedDrivingDistance -
        REMAINING_DISTANCE_AFTER_MODIFIED_CHARGING_HUB_STOP

    /* Sample driving speed (based on original driving distance) */
    val newDrivingSpeed: Velocity =
      drivingSpeed
        .sample(
          currentTime,
          plannedDrivingDistance,
        )

    /* Calculate driving time */
    val newDrivingTime: Time = Minutes(
      math.max(
        (math rint (newDrivingDistance / newDrivingSpeed).toMinutes).toInt,
        1,
      )
    )

    val newDestinationCategoricalLocation: CategoricalLocationDictionary.Value =
      PoiEnums.CategoricalLocationDictionary(chargingHubPoiType)
    /* Sample destination POI */
    val newDestinationPoi: PointOfInterest =
      poisWithSizes(
        newDestinationCategoricalLocation
      ).sample()

    /* Calculate start of parking time */
    val newParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(newDrivingTime.toMinutes.toLong)

    /* Calculate parking time */
    val newParkingTime: Time =
      calculateChargingTimeAtChargingHub(
        ev,
        newDestinationPoi,
        newStoredEnergyEndOfTrip,
        chargingStations,
      )

    /* Calculate departure time */
    val newDepartureTime: ZonedDateTime =
      newParkingTimeStart.plusMinutes(newParkingTime.toMinutes.toLong)

    /* Save parameters of the originally planned trip for the next trip simulation after charging */
    val updatedEv: ElectricVehicle = ev
      .setRemainingDistanceAfterChargingHub(
        Some(
          Quantities.getQuantity(
            (plannedDrivingDistance - newDrivingDistance).toKilometers,
            PowerSystemUnits.KILOMETRE,
          )
        )
      )
      .setFinalDestination(plannedDestinationPoi, plannedDestinationPoiType)

    /* Create updated EV */
    updatedEv.copyWith(
      Quantities
        .getQuantity(newStoredEnergyEndOfTrip.toKilowattHours, KILOWATTHOUR),
      newDestinationPoi,
      chargingHubPoiType,
      newParkingTimeStart,
      newDepartureTime,
    )
  }

  /** Decide whether EV makes a stop at a charging hub to recharge during the
    * trip and determines the SoC at arrival
    *
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param plannedStoredEnergyEndOfTrip
    *   stored energy at the end of the planned trip
    * @param plannedDestinationCategoricalLocation
    *   categorical location at the end of the planned trip
    * @param plannedParkingTimeStart
    *   parking time start of the planned trip
    * @param plannedDepartureTime
    *   departure time of the planned trip
    * @return
    *   Decision, whether EV wants to make a stop at a charging hub to recharge
    *   during the trip, and at which SoC
    */
  private def doesEvWantToChargeAtChargingHub(
      ev: ElectricVehicle,
      plannedStoredEnergyEndOfTrip: Energy,
      plannedDestinationCategoricalLocation: CategoricalLocationDictionary.Value,
      plannedParkingTimeStart: ZonedDateTime,
      plannedDepartureTime: ZonedDateTime,
  ): (Boolean, Option[Double]) = {

    val socAtStartOfTrip: Double =
      ev.getStoredEnergy.divide(ev.getEStorage).getValue.doubleValue()

    val socAtEndOfTrip: Double =
      plannedStoredEnergyEndOfTrip.toKilowattHours / ev.getEStorage
        .to(KILOWATTHOUR)
        .getValue
        .doubleValue()

    /* EV can be sufficiently charged if the next destination is home, home charging is possible and EV stays for at least 3 hours */
    val sufficientHomeChargingPossible: Boolean =
      (plannedDestinationCategoricalLocation == PoiEnums.CategoricalLocationDictionary.HOME
        && ev.chargingAtHomePossible
        && plannedParkingTimeStart.until(
          plannedDepartureTime,
          ChronoUnit.MINUTES,
        ) > 180)

    /* If state of charge is < 20% at end of trip and the EV cannot charge at home for at least 3 hours -> make stop at charging hub,
     *  or always, when SoC is < 5 % at end of trip */
    if (
      socAtEndOfTrip < SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD && !sufficientHomeChargingPossible
      || socAtEndOfTrip < 0.05
    ) {

      /* If SoC at start is higher than threshold, sample SoC at arrival at charging hub */
      if (socAtStartOfTrip >= SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD) {
        val socAtChargingHubArrival =
          SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD + seed
            .nextDouble() * (math.min(
            socAtStartOfTrip,
            SOC_OF_30_PERCENT,
          ) - SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD)
        (true, Some(socAtChargingHubArrival))
      }

      /* If SoC at start is already lower than threshold, assume the EV starts charging immediately at charging hub */
      else {
        (true, Some(socAtStartOfTrip))
      }

    } else (false, None)

  }

  /** Keep the planned trip and do not change any parameters of the trip. This
    * function might always be called, when there are no charging hubs in the
    * area which could prevent EVs from driving their battery to zero. In this
    * case, it might happen here that an EV drives to a SoC of zero.
    *
    * @param ev
    *   The EV for which the trip shall be simulated
    * @param plannedStoredEnergyEndOfTrip
    *   planned stored energy at end of the trip
    * @param plannedDestinationPoi
    *   planned destination POI
    * @param plannedParkingTimeStart
    *   planned parking time start
    * @param plannedDepartureTime
    *   planned departure time
    * @return
    */
  def keepOriginalTrip(
      ev: ElectricVehicle,
      plannedStoredEnergyEndOfTrip: Energy,
      plannedDestinationPoi: PointOfInterest,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      plannedParkingTimeStart: ZonedDateTime,
      plannedDepartureTime: ZonedDateTime,
  ): ElectricVehicle = {
    implicit val tolerance: Energy = WattHours(1e-6)

    /* Because there is no stop at a charging hub, no trip values need to be saved */
    val updatedEv: ElectricVehicle = ev
      .resetFinalDestination()
      .setRemainingDistanceAfterChargingHub(None)

    if (plannedStoredEnergyEndOfTrip =~ ZERO_ENERGY) {
      logger.info(
        s"${ev.getId} has driven its battery to SoC = 0%."
      )
    }

    /* Create updated EV */
    updatedEv.copyWith(
      Quantities.getQuantity(
        plannedStoredEnergyEndOfTrip.toKilowattHours,
        KILOWATTHOUR,
      ),
      plannedDestinationPoi,
      plannedDestinationPoiType,
      plannedParkingTimeStart,
      plannedDepartureTime,
    )
  }

  /** Calculate stored energy at the end of the trip based on the trip distance.
    * The minimum stored energy is zero, even if the trip is longer.
    *
    * @param consumption
    *   Consumption of the ev
    * @param storedEnergy
    *   Stored energy of the ev
    * @param drivingDistance
    *   The driving distance of the ev
    * @return
    *   stored energy at the end of the trip
    */
  def calculateStoredEnergyAtEndOfTrip(
      consumption: SpecificEnergyDistance,
      storedEnergy: Energy,
      drivingDistance: Length,
  ): Energy = {

    /* Calculate consumed energy during the trip */
    val consumedEnergy: Energy = drivingDistance.calcEnergy(consumption)
    /* Calculate storedEnergy at end of trip */
    if ((storedEnergy - consumedEnergy) < ZERO_ENERGY)
      ZERO_ENERGY
    else storedEnergy - consumedEnergy
  }

  /** Calculate departure time for the trip. The next departure might be sampled
    * for the next day if the EV is at home, or based on the previous POI type.
    *
    * @param destinationPoiType
    *   destination POI type for the trip
    * @param parkingTimeStart
    *   star tof parking time at the destination
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param lastTripOfDay
    *   Meta-information to determine if that trip is the last trip of the day
    * @param parkingTime
    *   Meta-information to determine the parking time
    * @return
    *   departure time for the trip
    */
  def calculateDepartureTime(
      destinationPoiType: PoiTypeDictionary.Value,
      parkingTimeStart: ZonedDateTime,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime,
  ): ZonedDateTime = {

    /** Sample departure time, if trip is not last trip of the day */
    def sampleDepartureTime(): ZonedDateTime = {
      /* Sample parking time */
      val pt = parkingTime.sample(parkingTimeStart, destinationPoiType)

      /* Calculate and return departure time of EV using the sampled parking time */
      parkingTimeStart.plusMinutes(pt)
    }

    /* Sample whether this trip is the last trip of day */
    val departureTime: ZonedDateTime = destinationPoiType match {
      /* if destination POI type is home */
      case HOME =>
        if (lastTripOfDay.sample(parkingTimeStart, seed)) {
          firstDepartureOfDay.sample(parkingTimeStart)
        } else {
          sampleDepartureTime()
        }
      case _ => sampleDepartureTime()
    }

    departureTime
  }

  /** Check if sampled driving distance is possible. Checks if EV can drive the
    * sampled distance with its current SOC, otherwise reduce distance to max
    * possible This check should only be necessary if there are no charging hubs
    * in the area where the EV would otherwise be sent ot recharge.
    *
    * @param sampledTripDistance
    *   sampled trip distance for the EV
    * @param ev
    *   The ev for which the trip should be checked
    * @return
    *   possibly adjusted driving distance for the trip
    */
  @deprecated(
    "Currently not used. EV is either sent to charging hub or has SoC=0% at end of trip and teleports for next trip"
  )
  def checkAndIfNecessaryAdjustDrivingDistance(
      sampledTripDistance: Length,
      ev: ElectricVehicle,
  ): Length = {

    val possibleDistance: Length = KilowattHours(
      ev.getStoredEnergy
        .to(KILOWATTHOUR)
        .getValue
        .doubleValue()
    ).calcDistance(ev.evType.consumption)
    val maxTripDistance: Length = {
      if (possibleDistance > ZERO_DISTANCE) possibleDistance
      else {
        logger.info(
          s"${ev.getId} cannot drive anymore because its battery is empty!"
        )
        ZERO_DISTANCE
      }
    }

    if (sampledTripDistance < maxTripDistance) sampledTripDistance
    else maxTripDistance
  }

  /** Calculate parking time at charging hub dependent on the rated power of EV
    * and the charging hub to charge the EV to 100%.
    *
    * @param ev
    *   The EV for which the parking time shall be calculated, containing
    *   information on the destination POI
    * @param chargingStations
    *   All charging stations in the area
    * @return
    *   Parking time [minutes] at the charging hub to charge the EV to 100%
    */
  private def calculateChargingTimeAtChargingHub(
      ev: ElectricVehicle,
      destinationPoi: PointOfInterest,
      storedEnergyAtChargingStart: Energy,
      chargingStations: Seq[ChargingStation],
  ): Time = {
    val chargingHub = destinationPoi.nearestChargingStations.headOption
      .flatMap { case (closestChargingStation, _) =>
        chargingStations.find(_ == closestChargingStation)
      }
      .getOrElse(
        throw TripException(
          s"The destination POI '$destinationPoi' has no charging hub nearby."
        )
      )

    val chargingPowerOfChargingHub =
      chargingHub.evcsType
        .getsRated()
        .to(KILOWATT)

    val availableChargingPowerForEV =
      chargingHub.evcsType.getElectricCurrentType match {
        case ElectricCurrentType.AC =>
          ev.getPRatedAC.min(chargingPowerOfChargingHub).to(KILOWATT)
        case ElectricCurrentType.DC =>
          ev.getPRatedDC.min(chargingPowerOfChargingHub).to(KILOWATT)
      }

    val energyUntilFullCharge: Energy = KilowattHours(
      ev.getEStorage
        .to(KILOWATTHOUR)
        .getValue
        .doubleValue() - storedEnergyAtChargingStart.toKilowattHours
    )

    val neededChargingTime: Time = Hours(
      energyUntilFullCharge.toKilowattHours / availableChargingPowerForEV
        .to(KILOWATT)
        .getValue
        .doubleValue()
    )

    neededChargingTime.max(Minutes(1))
  }

}
