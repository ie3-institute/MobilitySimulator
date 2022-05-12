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
  PoiTypeDictionary
}
import edu.ie3.mobsim.io.geodata.{PoiEnums, PointOfInterest}
import edu.ie3.mobsim.io.probabilities.{
  CategoricalLocation,
  DrivingSpeed,
  FirstDepartureOfDay,
  LastTripOfDay,
  ParkingTime,
  PoiTransition,
  ProbabilityDensityFunction,
  TripDistance
}
import edu.ie3.mobsim.utils.DefaultQuantities.{
  REMAINING_DISTANCE_AFTER_MODIFIED_CHARGING_HUB_STOP,
  SOC_OF_10_PERCENT,
  SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD,
  SOC_OF_30_PERCENT,
  SOC_OF_70_PERCENT,
  ZERO_DISTANCE,
  ZERO_ENERGY
}
import edu.ie3.mobsim.utils.IoUtils
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOMETRE,
  KILOWATT,
  KILOWATTHOUR
}
import edu.ie3.util.quantities.QuantityUtil
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.unit.Units.KILOMETRE_PER_HOUR

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import javax.measure.quantity.{Energy, Length, Speed}
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
    * @param categoricalLocation
    *   Needed meta information to determine next categorical location
    * @param drivingSpeed
    *   Needed meta information to determine next driving speed
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param lastTripOfDay
    *   Meta-information to determine if that trip is the last trip of the day
    * @param parkingTime
    *   Meta-information to determine the parking time
    * @param poiTransition
    *   Meta-information to determine the next POI transition
    * @param tripDistance
    *   Meta-information to determine the distance of the next trip
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
        ProbabilityDensityFunction[PointOfInterest]
      ],
      chargingHubTownIsPresent: Boolean,
      chargingHubHighwayIsPresent: Boolean,
      chargingStations: Set[ChargingStation],
      ioUtils: IoUtils,
      categoricalLocation: CategoricalLocation,
      drivingSpeed: DrivingSpeed,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime,
      poiTransition: PoiTransition,
      tripDistance: TripDistance,
      thresholdChargingHubDistance: ComparableQuantity[Length]
  ): ElectricVehicle = {

    /* Save EV to csv before trip */
    ioUtils.writeMovement(ev, currentTime, "departure")

    val socAtStartOfTrip: Double = ev.getStoredEnergy
      .to(KILOWATTHOUR)
      .divide(ev.getEStorage.to(KILOWATTHOUR))
      .getValue
      .doubleValue()

    val updatedEv: ElectricVehicle =
      /* Check if SoC is < 70% and EV parks at a charging hub -> if yes, let EV charge longer, do not depart */
      if (
        socAtStartOfTrip < SOC_OF_70_PERCENT &&
        (ev.getDestinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_TOWN || ev.getDestinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_HIGHWAY)
      ) {
        // logger.info(
        s"${ev.getId} has SoC < 70% at planned departure at charging hub, so it stays a bit longer..."
        // )
        doNotDepartAndStayLongerAtDestination(
          ev,
          currentTime,
          chargingStations,
          firstDepartureOfDay,
          lastTripOfDay,
          parkingTime
        )
      }

      /* Check if SoC is < 10% at start of trip -> if yes, and EV could charge at its destination, do not depart */
      else if (
        socAtStartOfTrip < SOC_OF_10_PERCENT
        && ev.getDestinationPoi.nearestChargingStations.nonEmpty
      ) {
        // logger.info(
        s"${ev.getId} has SoC < 10% at planned departure, but EV could charged here. It stays until it can charge..."
        // )
        doNotDepartAndStayLongerAtDestination(
          ev,
          currentTime,
          chargingStations,
          firstDepartureOfDay,
          lastTripOfDay,
          parkingTime
        )
      }

      /* SoC is not critical (< 10%) or couldn't charge at destination -> depart and simulate a trip */
      else {

        if (socAtStartOfTrip < SOC_OF_10_PERCENT) {
          // logger.info(
          s"${ev.getId} departs with battery SoC < 10% because it can't charge here. It might need to teleport to its next destination..."
          // )
        }

        val categoricalLocationToPdf = poisWithSizes.map {
          case (categoricalLocation, pdf) =>
            categoricalLocation -> pdf
        }
        /* Get planned POI information and distance for the trip */
        val (
          plannedDestinationPoiType,
          plannedDestinationCategoricalLocation,
          plannedDestinationPoi,
          plannedDrivingDistance
        ) = getTargetProperties(
          ev,
          currentTime,
          categoricalLocationToPdf,
          categoricalLocation,
          poiTransition,
          tripDistance
        ) match {
          case TargetProperties(
                poiType,
                categoricalLocation,
                poi,
                distance
              ) =>
            (poiType, categoricalLocation, poi, distance)
        }

        /* Simulate the planned trip */
        val (
          plannedStoredEnergyEndOfTrip,
          plannedParkingTimeStart,
          plannedDepartureTime
        ) = simulatePlannedTrip(
          ev,
          currentTime,
          plannedDrivingDistance,
          plannedDestinationPoiType,
          drivingSpeed,
          firstDepartureOfDay,
          lastTripOfDay,
          parkingTime
        )

        /* Decide whether EV makes a stop at a charging hub to recharge during the trip */
        val (evWantsToChargeAtChargingHub, maybeSocAtArrival) =
          doesEvWantToChargeAtChargingHub(
            ev,
            plannedStoredEnergyEndOfTrip,
            plannedDestinationCategoricalLocation,
            plannedParkingTimeStart,
            plannedDepartureTime
          )

        maybeSocAtArrival match {
          case Some(socAtArrival) if evWantsToChargeAtChargingHub =>
            (
              plannedDrivingDistance.isLessThanOrEqualTo(
                thresholdChargingHubDistance
              ),
              chargingHubTownIsPresent,
              chargingHubHighwayIsPresent
            ) match {
              case (true, true, _) =>
                // logger.info(
                s"${ev.getId} drives to a charging hub of type chargingHubTown."
                // )
                makeTripToChargingHub(
                  "chargingHubTown",
                  ev,
                  currentTime,
                  poisWithSizes,
                  socAtStartOfTrip,
                  socAtArrival,
                  plannedDrivingDistance,
                  plannedDestinationPoi,
                  plannedDestinationPoiType,
                  chargingStations,
                  drivingSpeed
                )
              case (true, false, _) =>
                // logger.info(
                s"${ev.getId} wants to charge at charging hub on a <50km trip, but keeps its original trip because there are no chargingHubTown."
                // )
                keepOriginalTrip(
                  ev,
                  plannedStoredEnergyEndOfTrip,
                  plannedDestinationPoiType,
                  plannedDestinationCategoricalLocation,
                  plannedDestinationPoi,
                  plannedParkingTimeStart,
                  plannedDepartureTime
                )
              case (false, _, true) =>
                // logger.info(
                s"${ev.getId} drives to a charging hub of type chargingHubHighway."
                // )
                makeTripToChargingHub(
                  "chargingHubHighway",
                  ev,
                  currentTime,
                  poisWithSizes,
                  socAtStartOfTrip,
                  socAtArrival,
                  plannedDrivingDistance,
                  plannedDestinationPoi,
                  plannedDestinationPoiType,
                  chargingStations,
                  drivingSpeed
                )
              case (false, true, false) =>
                // logger.info(
                s"${ev.getId} drives to a charging hub of type chargingHubTown. " +
                  s"This is a modified trip, because there are no highway charging hubs."
                // )
                makeModifiedTripToChargingHub(
                  "chargingHubTown",
                  ev,
                  currentTime,
                  poisWithSizes,
                  socAtArrival,
                  plannedDrivingDistance,
                  plannedDestinationPoi,
                  plannedDestinationPoiType,
                  chargingStations,
                  drivingSpeed
                )
              case (false, false, false) =>
                // logger.info(
                s"${ev.getId} wants to charge at charging hub on a >50km trip, but keeps its original trip because there are no chargingHubs at all."
                // )
                keepOriginalTrip(
                  ev,
                  plannedStoredEnergyEndOfTrip,
                  plannedDestinationPoiType,
                  plannedDestinationCategoricalLocation,
                  plannedDestinationPoi,
                  plannedParkingTimeStart,
                  plannedDepartureTime
                )
            }
          case _ =>
            /* Do not change the planned trip */
            // logger.info(
            s"${ev.getId} makes its planned trip."
            // )
            keepOriginalTrip(
              ev,
              plannedStoredEnergyEndOfTrip,
              plannedDestinationPoiType,
              plannedDestinationCategoricalLocation,
              plannedDestinationPoi,
              plannedParkingTimeStart,
              plannedDepartureTime
            )
        }
      }

    updatedEv
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
        ProbabilityDensityFunction[PointOfInterest]
      ],
      categoricalLocation: CategoricalLocation,
      poiTransition: PoiTransition,
      tripDistance: TripDistance
  ): TargetProperties = {
    val currentPoiType = ev.getDestinationPoiType
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
        tripDistance
      )
    }
  }

  /** Properties of a target for a trip
    *
    * @param poiType
    *   Type of the POI
    * @param categoricalLocation
    *   Type of categorical location
    * @param poi
    *   Actual target POI
    * @param distance
    *   Distance to be driven
    */
  private final case class TargetProperties(
      poiType: PoiTypeDictionary.Value,
      categoricalLocation: CategoricalLocationDictionary.Value,
      poi: PointOfInterest,
      distance: ComparableQuantity[Length]
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
      ev.getFinalDestinationPoiType,
      ev.getFinalDestinationPoi,
      ev.getRemainingDistanceAfterChargingHub
    ) match {
      case (
            Some(destinationPoiType),
            Some(destinationPoi),
            Some(remainingDistance)
          ) =>
        /* Reset saved values */
        ev.setFinalDestinationPoiType(None)
        ev.setFinalDestinationPoi(None)
        ev.setRemainingDistanceAfterChargingHub(None)

        /* Return the determined values */
        TargetProperties(
          destinationPoiType,
          destinationPoi.categoricalLocation,
          destinationPoi,
          remainingDistance
        )
      case (None, _, _) =>
        throw TripException(
          "Cannot resume trip, as the previous destination POI type is unknown."
        )
      case (Some(_), None, _) =>
        throw TripException(
          "Cannot resume trip, as the previous destination POI is unknown."
        )
      case (Some(_), Some(_), None) =>
        throw TripException(
          "Cannot resume trip, as the remaining distance is unknown."
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
        ]
      ],
      poiTransition: PoiTransition,
      tripDistance: TripDistance
  ): TargetProperties = {
    /* Save previous POI type (is required for later calculations) */
    val previousPoiType = ev.getDestinationPoiType

    /* Sample next destination POI type */
    val destinationPoiType =
      poiTransition.sample(time, previousPoiType)

    /* Sample specific destination POI instance */
    sampleNextPoi(
      destinationPoiType,
      time,
      ev.getHomePOI,
      ev.getWorkPOI,
      categoricalLocation,
      categoricalLocationToPdf
    ) match {
      case (categoricalLocation, poi) =>
        /* Sample driving distance */
        val drivingDistance: ComparableQuantity[Length] =
          tripDistance.sample(
            time,
            previousPoiType,
            destinationPoiType
          )
        TargetProperties(
          destinationPoiType,
          categoricalLocation,
          poi,
          drivingDistance
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
    *   An optional tuple of categorical location and actual POI
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
        ]
      ]
  ): (PoiEnums.CategoricalLocationDictionary.Value, PointOfInterest) = {
    destinationPoiType match {
      case HOME =>
        /* POI type "Home" is directly mapped to a fixed home POI */
        (CategoricalLocationDictionary.HOME, homePoi)
      case WORK =>
        /* POI type "Work" is directly mapped to a fixed work POI */
        (CategoricalLocationDictionary.WORK, workPoi)
      case _ =>
        /* Any other POI type. Sample a random categorical location and based on that a specific POI. */
        sampleDestinationCategoricalLocationAndPoi(
          time,
          destinationPoiType,
          categoricalLocation,
          categoricalLocationToPdf
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
    *   A pair of categorical location and actual destination POI
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
        ]
      ],
      depth: Int = 0,
      maxDepth: Int = 4
  ): (PoiEnums.CategoricalLocationDictionary.Value, PointOfInterest) = {
    val nextCategoricalLocation =
      categoricalLocation.sample(time, destinationPoiType)
    categoricalLocationToPdf.get(nextCategoricalLocation) match {
      case Some(pdf) => nextCategoricalLocation -> pdf.sample()
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
            maxDepth
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
      plannedDrivingDistance: ComparableQuantity[Length],
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      drivingSpeed: DrivingSpeed,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime
  ): (ComparableQuantity[Energy], ZonedDateTime, ZonedDateTime) = {

    /* Calculate stored energy at the end of the trip based on planned values */
    val plannedStoredEnergyEndOfTrip: ComparableQuantity[Energy] =
      calculateStoredEnergyAtEndOfTrip(
        ev: ElectricVehicle,
        plannedDrivingDistance: ComparableQuantity[Length]
      )

    /* Sample driving speed based on planned values */
    val plannedDrivingSpeed = drivingSpeed
      .sample(currentTime, plannedDrivingDistance)
      .to(KILOMETRE_PER_HOUR)

    /* Calculate driving time based on planned values */
    val plannedDrivingTime: Int = math.max(
      (math rint (plannedDrivingDistance
        .to(KILOMETRE)
        .divide(plannedDrivingSpeed.to(KILOMETRE_PER_HOUR))
        .getValue
        .doubleValue() * 60)).toInt,
      1
    )

    /* Calculate start of parking time based on planned values */
    val plannedParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(plannedDrivingTime)

    /* Calculate departure time based on planned values */
    val plannedDepartureTime: ZonedDateTime = calculateDepartureTime(
      plannedDestinationPoiType,
      plannedParkingTimeStart,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime
    )

    (
      plannedStoredEnergyEndOfTrip,
      plannedParkingTimeStart,
      plannedDepartureTime
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
      chargingHubPoiType: String,
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      poisWithSizes: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest]
      ],
      socAtStartOfTrip: Double,
      socAtChargingHubArrival: Double,
      plannedDrivingDistance: ComparableQuantity[Length],
      plannedDestinationPoi: PointOfInterest,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      chargingStations: Set[ChargingStation],
      drivingSpeed: DrivingSpeed
  ): ElectricVehicle = {

    /* Drive only until SoC reaches SoC of charging hub arrival */
    val newStoredEnergyEndOfTrip: ComparableQuantity[Energy] =
      ev.getEStorage.multiply(socAtChargingHubArrival)

    /* Reduced used energy for the trip until the charging hub */
    val usedEnergyForThisTrip: ComparableQuantity[Energy] =
      ev.getStoredEnergy.subtract(newStoredEnergyEndOfTrip)

    /* Reduced driving distance to the charging hub */
    val newDrivingDistance: ComparableQuantity[Length] = usedEnergyForThisTrip
      .divide(ev.getConsumption)
      .asType(classOf[Length])
      .to(KILOMETRE)

    /* Sample driving speed (based on original driving distance) */
    val newDrivingSpeed: ComparableQuantity[Speed] =
      drivingSpeed
        .sample(
          currentTime,
          plannedDrivingDistance
        )
        .to(KILOMETRE_PER_HOUR)

    /* Calculate driving time */
    val newDrivingTime: Int = math.max(
      (math rint (newDrivingDistance
        .to(KILOMETRE)
        .divide(newDrivingSpeed.to(KILOMETRE_PER_HOUR))
        .getValue
        .doubleValue() * 60)).toInt,
      1
    )

    /* Calculate start of parking time */
    val newParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(newDrivingTime)

    val newDestinationPoiType: PoiTypeDictionary.Value =
      PoiEnums.PoiTypeDictionary(chargingHubPoiType)
    val newDestinationCategoricalLocation: CategoricalLocationDictionary.Value =
      PoiEnums.CategoricalLocationDictionary(chargingHubPoiType)
    /* Sample destination POI */
    val newDestinationPoi: PointOfInterest =
      poisWithSizes(
        newDestinationCategoricalLocation
      ).sample()

    /* Calculate parking time */
    val newParkingTime: Long =
      calculateChargingTimeAtChargingHub(
        ev,
        newDestinationPoi,
        newStoredEnergyEndOfTrip,
        chargingStations
      )

    /* Calculate departure time */
    val newDepartureTime: ZonedDateTime =
      newParkingTimeStart.plusMinutes(newParkingTime)

    /* Save parameters of the originally planned trip for the next trip simulation after charging */
    ev.setRemainingDistanceAfterChargingHub(
      Some(plannedDrivingDistance.subtract(newDrivingDistance))
    )
    ev.setFinalDestinationPoi(Some(plannedDestinationPoi))
    ev.setFinalDestinationPoiType(Some(plannedDestinationPoiType))

    /* Create updated EV */
    ev.copyWith(
      newStoredEnergyEndOfTrip,
      newDestinationPoiType,
      newDestinationCategoricalLocation,
      newDestinationPoi,
      newParkingTimeStart,
      newDepartureTime
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
    * @param plannedDestinationPoiType
    *   planned destination POI type
    * @param drivingSpeed
    *   Meta-information to determine the next driving speed
    * @return
    */
  def makeModifiedTripToChargingHub(
      chargingHubPoiType: String,
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      poisWithSizes: Map[
        CategoricalLocationDictionary.Value,
        ProbabilityDensityFunction[PointOfInterest]
      ],
      socAtChargingHubArrival: Double,
      plannedDrivingDistance: ComparableQuantity[Length],
      plannedDestinationPoi: PointOfInterest,
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      chargingStations: Set[ChargingStation],
      drivingSpeed: DrivingSpeed
  ): ElectricVehicle = {

    /* Set SoC at arriving at charging hub in town */
    val newStoredEnergyEndOfTrip: ComparableQuantity[Energy] =
      ev.getEStorage.multiply(socAtChargingHubArrival)

    /* Set driving distance to the charging hub to fixed value, so that only 10 km are left afterwards */
    val newDrivingDistance: ComparableQuantity[Length] =
      plannedDrivingDistance.subtract(
        REMAINING_DISTANCE_AFTER_MODIFIED_CHARGING_HUB_STOP
      )

    /* Sample driving speed (based on original driving distance) */
    val newDrivingSpeed: ComparableQuantity[Speed] =
      drivingSpeed
        .sample(
          currentTime,
          plannedDrivingDistance
        )
        .to(KILOMETRE_PER_HOUR)

    /* Calculate driving time */
    val newDrivingTime: Int = math.max(
      (math rint (newDrivingDistance
        .to(KILOMETRE)
        .divide(newDrivingSpeed.to(KILOMETRE_PER_HOUR))
        .getValue
        .doubleValue() * 60)).toInt,
      1
    )

    val newDestinationPoiType: PoiTypeDictionary.Value =
      PoiEnums.PoiTypeDictionary(chargingHubPoiType)
    val newDestinationCategoricalLocation: CategoricalLocationDictionary.Value =
      PoiEnums.CategoricalLocationDictionary(chargingHubPoiType)
    /* Sample destination POI */
    val newDestinationPoi: PointOfInterest =
      poisWithSizes(
        newDestinationCategoricalLocation
      ).sample()

    /* Calculate start of parking time */
    val newParkingTimeStart: ZonedDateTime =
      currentTime.plusMinutes(newDrivingTime)

    /* Calculate parking time */
    val newParkingTime: Long =
      calculateChargingTimeAtChargingHub(
        ev,
        newDestinationPoi,
        newStoredEnergyEndOfTrip,
        chargingStations
      )

    /* Calculate departure time */
    val newDepartureTime: ZonedDateTime =
      newParkingTimeStart.plusMinutes(newParkingTime)

    /* Save parameters of the originally planned trip for the next trip simulation after charging */
    ev.setRemainingDistanceAfterChargingHub(
      Some(plannedDrivingDistance.subtract(newDrivingDistance))
    )
    ev.setFinalDestinationPoi(Some(plannedDestinationPoi))
    ev.setFinalDestinationPoiType(Some(plannedDestinationPoiType))

    /* Create updated EV */
    ev.copyWith(
      newStoredEnergyEndOfTrip,
      newDestinationPoiType,
      newDestinationCategoricalLocation,
      newDestinationPoi,
      newParkingTimeStart,
      newDepartureTime
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
      plannedStoredEnergyEndOfTrip: ComparableQuantity[Energy],
      plannedDestinationCategoricalLocation: CategoricalLocationDictionary.Value,
      plannedParkingTimeStart: ZonedDateTime,
      plannedDepartureTime: ZonedDateTime
  ): (Boolean, Option[Double]) = {

    val socAtStartOfTrip: Double = ev.getStoredEnergy
      .to(KILOWATTHOUR)
      .divide(ev.getEStorage.to(KILOWATTHOUR))
      .getValue
      .doubleValue()

    val socAtEndOfTrip: Double = plannedStoredEnergyEndOfTrip
      .to(KILOWATTHOUR)
      .divide(ev.getEStorage.to(KILOWATTHOUR))
      .getValue
      .doubleValue()

    /* EV can be sufficiently charged if the next destination is home, home charging is possible and EV stays for at least 3 hours */
    val sufficientHomeChargingPossible: Boolean =
      (plannedDestinationCategoricalLocation == PoiEnums
        .CategoricalLocationDictionary("home")
        && ev.isChargingAtHomePossible
        && plannedParkingTimeStart.until(
          plannedDepartureTime,
          ChronoUnit.MINUTES
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
            SOC_OF_30_PERCENT
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
    * @param plannedDestinationPoiType
    *   planned destination POI type
    * @param plannedDestinationCategoricalLocation
    *   planned destination categorical location
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
      plannedStoredEnergyEndOfTrip: ComparableQuantity[Energy],
      plannedDestinationPoiType: PoiTypeDictionary.Value,
      plannedDestinationCategoricalLocation: CategoricalLocationDictionary.Value,
      plannedDestinationPoi: PointOfInterest,
      plannedParkingTimeStart: ZonedDateTime,
      plannedDepartureTime: ZonedDateTime
  ): ElectricVehicle = {

    /* Because there is no stop at a charging hub, no trip values need to be saved */
    ev.setFinalDestinationPoiType(None)
    ev.setFinalDestinationPoi(None)
    ev.setRemainingDistanceAfterChargingHub(None)

    if (
      QuantityUtil.isEquivalentAbs(
        plannedStoredEnergyEndOfTrip,
        ZERO_ENERGY,
        1
      )
    ) {
      logger.info(
        s"${ev.getId} has driven its battery to SoC = 0%."
      )
    }

    /* Create updated EV */
    ev.copyWith(
      plannedStoredEnergyEndOfTrip,
      plannedDestinationPoiType,
      plannedDestinationCategoricalLocation,
      plannedDestinationPoi,
      plannedParkingTimeStart,
      plannedDepartureTime
    )
  }

  /** Create updated EV for the case that the EV does not depart because its SoC
    * is too low and it has the possibility to charge at its current POI.
    *
    * @param ev
    *   The EV which shall be updated
    * @param currentTime
    *   current time
    * @param chargingStations
    *   Collection of available charging stations
    * @param firstDepartureOfDay
    *   Meta-information to determine the first departure of the day
    * @param lastTripOfDay
    *   Meta-information to determine if that trip is the last trip of the day
    * @param parkingTime
    *   Meta-information to determine the parking time
    * @return
    *   The updated EV with only updated parking and departure times
    */
  private def doNotDepartAndStayLongerAtDestination(
      ev: ElectricVehicle,
      currentTime: ZonedDateTime,
      chargingStations: Set[ChargingStation],
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime
  ): ElectricVehicle = {

    val parkingTimeStart: ZonedDateTime = currentTime.plusMinutes(1)
    val departureTime: ZonedDateTime =
      if (
        ev.getDestinationPoiType == PoiTypeDictionary.CHARGING_HUB_TOWN
        || ev.getDestinationPoiType == PoiTypeDictionary.CHARGING_HUB_HIGHWAY
      ) {
        parkingTimeStart.plusMinutes(
          calculateChargingTimeAtChargingHub(
            ev,
            ev.getDestinationPoi,
            ev.getStoredEnergy,
            chargingStations
          )
        )
      } else {
        calculateDepartureTime(
          ev.getDestinationPoiType,
          parkingTimeStart,
          firstDepartureOfDay,
          lastTripOfDay,
          parkingTime
        )
      }

    /* Create updated EV */
    ev.copyWith(
      ev.getStoredEnergy,
      ev.getDestinationPoiType,
      ev.getDestinationCategoricalLocation,
      ev.getDestinationPoi,
      parkingTimeStart,
      departureTime
    )
  }

  /** Calculate stored energy at the end of the trip based on the trip distance.
    * The minimum stored energy is zero, even if the trip is longer.
    *
    * @param ev
    *   The ev for which the stored energy shall be calculated
    * @param drivingDistance
    *   The driving distance of the ev
    * @return
    *   stored energy at the end of the trip
    */
  def calculateStoredEnergyAtEndOfTrip(
      ev: ElectricVehicle,
      drivingDistance: ComparableQuantity[Length]
  ): ComparableQuantity[Energy] = {

    /* Calculate consumed energy during the trip */
    val consumedEnergy: ComparableQuantity[Energy] = drivingDistance
      .multiply(ev.getConsumption)
      .asType(classOf[Energy])
      .to(KILOWATTHOUR)

    /* Calculate storedEnergy at end of trip */
    if (ev.getStoredEnergy.subtract(consumedEnergy).isLessThan(ZERO_ENERGY))
      ZERO_ENERGY
    else ev.getStoredEnergy.subtract(consumedEnergy)
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
      parkingTime: ParkingTime
  ): ZonedDateTime = {

    /** Sample departure time, if trip is not last trip of the day */
    def sampleDepartureTime(): ZonedDateTime = {
      /* Sample parking time */
      val pt = parkingTime.sample(parkingTimeStart, destinationPoiType)
      // logger.info(s"Parking time: $parkingTime")

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
      sampledTripDistance: ComparableQuantity[Length],
      ev: ElectricVehicle
  ): ComparableQuantity[Length] = {

    val possibleDistance: ComparableQuantity[Length] = ev.getStoredEnergy
      .divide(ev.getConsumption)
      .asType(classOf[Length])
      .to(KILOMETRE)

    val maxTripDistance: ComparableQuantity[Length] = {
      if (possibleDistance.isGreaterThan(ZERO_DISTANCE)) possibleDistance
      else {
        logger.info(
          s"${ev.getId} cannot drive anymore because its battery is empty!"
        )
        ZERO_DISTANCE
      }
    }

    if (sampledTripDistance.isLessThan(maxTripDistance)) sampledTripDistance
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
      storedEnergyAtChargingStart: ComparableQuantity[Energy],
      chargingStations: Set[ChargingStation]
  ): Long = {
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
      chargingHub.getEvcsType
        .getsRated()
        .to(KILOWATT)

    val availableChargingPowerForEV =
      chargingHub.getEvcsType.getElectricCurrentType match {
        case ElectricCurrentType.AC =>
          ev.getSRatedAC.min(chargingPowerOfChargingHub).to(KILOWATT)
        case ElectricCurrentType.DC =>
          ev.getSRatedDC.min(chargingPowerOfChargingHub).to(KILOWATT)
      }

    val energyUntilFullCharge =
      ev.getEStorage.subtract(storedEnergyAtChargingStart).to(KILOWATTHOUR)

    val neededChargingTimeInMinutes = (energyUntilFullCharge
      .divide(availableChargingPowerForEV)
      .getValue
      .doubleValue() * 60).toLong

    math.max(neededChargingTimeInMinutes, 1)
  }

}
