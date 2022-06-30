/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.MobilitySimulator.Movement
import edu.ie3.mobsim.config.{ArgsParser, ConfigFailFast}
import edu.ie3.mobsim.exceptions.{
  InitializationException,
  SourceException,
  UninitializedException
}
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.{PoiUtils, PointOfInterest}
import edu.ie3.mobsim.io.model.EvTypeInput
import edu.ie3.mobsim.io.probabilities._
import edu.ie3.mobsim.io.probabilities.factories.{
  CategoricalLocationFactory,
  DrivingSpeedFactory,
  FirstDepartureFactory,
  LastTripFactory,
  ParkingTimeFactory,
  PoiTransitionFactory,
  TripDistanceFactory
}
import edu.ie3.mobsim.model.ChargingBehavior.chooseChargingStation
import edu.ie3.mobsim.model.TripSimulation.simulateNextTrip
import edu.ie3.mobsim.model.{ChargingStation, ElectricVehicle}
import edu.ie3.mobsim.utils.{IoUtils, PathsAndSources}
import edu.ie3.simona.api.data.ExtDataSimulation
import edu.ie3.simona.api.data.ev.ontology.builder.EvMovementsMessageBuilder
import edu.ie3.simona.api.data.ev.{ExtEvData, ExtEvSimulation}
import edu.ie3.simona.api.simulation.ExtSimulation
import edu.ie3.util.TimeUtil
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.temporal.ChronoUnit
import java.time.{ZoneId, ZonedDateTime}
import java.util.UUID
import javax.measure.quantity.Length
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Random, Success}

final class MobilitySimulator(
    evData: ExtEvData,
    chargingStations: Set[ChargingStation],
    poisWithSizes: Map[
      CategoricalLocationDictionary.Value,
      ProbabilityDensityFunction[PointOfInterest]
    ],
    startTime: ZonedDateTime,
    var electricVehicles: SortedSet[ElectricVehicle],
    chargingHubTownIsPresent: Boolean,
    chargingHubHighwayIsPresent: Boolean,
    ioUtils: IoUtils,
    categoricalLocation: CategoricalLocation,
    drivingSpeed: DrivingSpeed,
    firstDepartureOfDay: FirstDepartureOfDay,
    lastTripOfDay: LastTripOfDay,
    parkingTime: ParkingTime,
    poiTransition: PoiTransition,
    tripDistance: TripDistance,
    maxDistanceFromPoi: ComparableQuantity[Length],
    thresholdChargingHubDistance: ComparableQuantity[Length]
) extends LazyLogging {
  def doActivity(tick: Long): java.util.ArrayList[java.lang.Long] = {
    /* Update current time */
    val currentTime = startTime.plusSeconds(tick)

    logger.info(
      s"Simulation triggered with tick: $tick -- Current time: $currentTime"
    )

    /* Receive available charging points of evcs from SIMONA */
    val availableChargingPoints: Map[UUID, Integer] =
      evData.requestAvailablePublicEvCs().asScala.toMap

    /* Receive current prices for public evcs situation */
    val currentPricesAtChargingStations: Map[UUID, java.lang.Double] =
      evData.requestCurrentPrices().asScala.toMap

    /* Send EV movements to SIMONA and receive charged EVs that ended parking */
    val (
      departedEvsFromSimona: SortedSet[ElectricVehicle],
      availableChargingPointsAfterMovements: Map[UUID, Integer]
    ) = sendEvMovementsToSimona(
      currentTime,
      availableChargingPoints,
      currentPricesAtChargingStations,
      maxDistanceFromPoi
    )

    /* Update and simulate all EVs that were returned from SIMONA */
    updateAndSimulateDepartedEvs(
      currentTime,
      departedEvsFromSimona,
      categoricalLocation,
      drivingSpeed,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime,
      poiTransition,
      tripDistance,
      thresholdChargingHubDistance
    )

    /* Get time until next event for one of the EVs and return corresponding tick to SIMONA */
    val timeUntilNextEvent =
      getTimeUntilNextEvent(electricVehicles, currentTime)

    /* Save occupancy of charging stations for csv output */
    chargingStations.foreach(cs =>
      ioUtils.writeEvcs(
        cs,
        availableChargingPointsAfterMovements,
        currentTime
      )
    )

    val newTicks = new java.util.ArrayList[java.lang.Long](1)
    newTicks.add(tick + timeUntilNextEvent)
    newTicks
  }

  /** Handover EVs to SIMONA which arrive at their destination POI or depart
    * from their POI at current time, but only if there is a charging point
    * available at the charging station. Otherwise, the EV is not sent to
    * SIMONA. <p> Get updated EV models for all EVs that depart from a charging
    * station and are returned from SIMONA.
    *
    * @param currentTime
    *   current time of the simulation
    * @param availableChargingPoints
    *   Map with currently available charging points at the evcs
    * @param currentPricesAtChargingStations
    *   Currently known prices per charging station
    * @param maxDistance
    *   Maximum permissible distance between a POI and a charging station
    * @return
    *   Returned EV models from SIMONA as set of Electric Vehicles
    */
  private def sendEvMovementsToSimona(
      currentTime: ZonedDateTime,
      availableChargingPoints: Map[UUID, Integer],
      currentPricesAtChargingStations: Map[UUID, java.lang.Double],
      maxDistance: ComparableQuantity[Length]
  ): (SortedSet[ElectricVehicle], Map[UUID, Integer]) = {
    val builder = new EvMovementsMessageBuilder

    /* Determine parking and departing evs in this tick */
    val (parkingEvs, departingEvs) =
      defineMovements(electricVehicles, currentTime)

    /* !!! Attention - Departing and parking evs have to be treated sequentially !!!
     * The departing cars offer additional free lots to the parking cars */
    val updatedChargingPoints =
      handleDepartures(departingEvs, availableChargingPoints, builder)

    /* Add EVs that start parking to movements and assign to Evcs UUID */
    val (takenChargingPoints, arrivals) = handleParkingEvs(
      parkingEvs,
      currentPricesAtChargingStations,
      updatedChargingPoints,
      maxDistance
    )
    arrivals.foreach { case Movement(cs, ev) =>
      builder.addArrival(cs, ev)
    }
    val finalTakenChargingPoints =
      updateFreeLots(updatedChargingPoints, takenChargingPoints)

    /* Send to SIMONA and receive departed EVs */
    val movements = builder.build()
    val departedEvs =
      evData
        .sendEvPositions(movements)
        .asScala
        .map(ev => ev.asInstanceOf[ElectricVehicle])
        .toSet
    departedEvs.foreach(ev =>
      ioUtils.writeMovement(ev, currentTime, "departure")
    )
    val sortedSet = SortedSet
      .empty[ElectricVehicle] ++ departedEvs
    (sortedSet, finalTakenChargingPoints)
  }

  /** Determine the set of cars, that start to park and that depart in this time
    * stamp
    *
    * @param evs
    *   Collection of known evs
    * @param currentTime
    *   The current wall-clock time
    * @return
    *   both sets
    */
  private def defineMovements(
      evs: SortedSet[ElectricVehicle],
      currentTime: ZonedDateTime
  ): (SortedSet[ElectricVehicle], SortedSet[ElectricVehicle]) = {
    val isParking = (ev: ElectricVehicle) =>
      ev.getParkingTimeStart == currentTime
    /* Relevant are only cars, that depart AND that are charging at a suitable charging station in SIMONA */
    val isDeparting = (ev: ElectricVehicle) =>
      ev.getDepartureTime == currentTime && ev.isChargingAtSimona && ev.getChosenChargingStation.isDefined
    evs.par
      .filter { ev =>
        isDeparting(ev) || isParking(ev)
      }
      .partition(_.getParkingTimeStart == currentTime) match {
      case (arrivals, departures) =>
        (TreeSet.from(arrivals), TreeSet.from(departures))
    }
  }

  /** Handle all ev departures
    *
    * @param evs
    *   Collection of departing evs
    * @param availableChargingPoints
    *   Overview of free charging points per charging station
    * @param builder
    *   Builder for ev movements message
    * @return
    *   An updated overview of free charging lots per charging station
    */
  private def handleDepartures(
      evs: Set[ElectricVehicle],
      availableChargingPoints: Map[UUID, Integer],
      builder: EvMovementsMessageBuilder
  ): Map[UUID, Integer] = {
    val (additionallyFreeChargingPoints, departures) =
      handleDepartingEvs(evs)
    departures.foreach { case Movement(cs, ev) =>
      builder.addDeparture(cs, ev.getUuid)
    }
    updateFreeLots(availableChargingPoints, additionallyFreeChargingPoints)
  }

  /** Handle all departing evs. If they are charging in SIMONA, determine the
    * charging station and count the amount of now additionally free charging
    * points.
    *
    * @param evs
    *   collection of departing evs
    * @return
    *   A mapping from charging station UUID to additionally freed charging
    *   points as well as single departures
    */
  private def handleDepartingEvs(
      evs: Set[ElectricVehicle]
  ): (Map[UUID, Integer], Seq[Movement]) =
    Await.result(
      Future
        .sequence(evs.toSeq.map(handleDepartingEv))
        .map {
          _.flatten.unzip match {
            case (uuids, movements) =>
              (
                uuids.groupBy(identity).map { case (uuid, uuids) =>
                  uuid -> uuids.size
                },
                movements
              )
          }
        },
      Duration("1h")
    )

  /** Handle a single departing ev. Empty information are handed back, if the
    * car either does not charge in SIMONA or there is no target charging
    * station defined.
    *
    * @param ev
    *   The car to handle
    * @return
    *   An [[Future]] onto an [[Option]] of the target charging stations' UUID
    *   as well as information about the movement
    */
  private def handleDepartingEv(
      ev: ElectricVehicle
  ): Future[Option[(UUID, Movement)]] =
    Future {
      /* Determine the charging station, the car currently is connected to */
      ev.getChosenChargingStation.map { cs =>
        /* Register departure */
        val movement = MobilitySimulator.Movement(cs, ev)
        logger.debug(
          s"${ev.getId} departs from $cs."
        )
        cs -> movement
      } match {
        case result @ Some(_) =>
          ev.removeChargingAtSimona()
          ev.setChosenChargingStation(None)
          result
        case result @ None =>
          logger.warn(
            s"Ev '$ev' is meant to charge in SIMONA, but no charging station is set."
          )
          result
      }
    }

  /** Update the free charging lots per charging station.
    *
    * @param freeLots
    *   Free charging points
    * @param change
    *   Difference per charging point
    * @return
    *   Updated overview of free lots
    */
  private def updateFreeLots(
      freeLots: Map[UUID, Integer],
      change: Map[UUID, Integer]
  ): Map[UUID, Integer] =
    freeLots.map { case (uuid, freeLots) =>
      val lotDiff: Integer = change.getOrElse(uuid, 0)
      val newLots: Integer = freeLots + lotDiff
      uuid -> newLots
    } ++ change
      .filter { case (uuid, _) =>
        !freeLots.keys.toSeq.contains(uuid)
      }
      .map { uuidToDifference =>
        val uuid = uuidToDifference._1
        val lotDifference: Integer = uuidToDifference._2
        uuid -> lotDifference
      }

  /** Handle parking evs. Based on given surrounding information, a target
    * charging station is sampled. The newly taken charging points as well as
    * the movements are tracked and handed back.
    *
    * @param evs
    *   Collection of parking evs
    * @param pricesAtChargingStation
    *   Prices at known charging stations
    * @param availableChargingPoints
    *   Available charging points per charging station
    * @param maxDistance
    *   Maximum permissible distance between an point of interest and a
    *   candidate charging station
    * @return
    *   An overview of newly taken charging points as well as a collection of
    *   movements
    */
  private def handleParkingEvs(
      evs: SortedSet[ElectricVehicle],
      pricesAtChargingStation: Map[UUID, java.lang.Double],
      availableChargingPoints: Map[UUID, Integer],
      maxDistance: ComparableQuantity[Length]
  ): (Map[UUID, Integer], Seq[Movement]) =
    Await.result(
      Future
        .sequence {
          evs.toSeq.map {
            /* Lets the EV choose whether and at which charging station it wants to charge */
            handleArrivingEv(
              _,
              pricesAtChargingStation,
              availableChargingPoints,
              maxDistance
            )
          }
        }
        .map {
          _.flatten.unzip match {
            case (uuids, movements) =>
              (
                uuids.groupBy(uuid => uuid).map { case (uuid, uuids) =>
                  uuid -> -uuids.size
                },
                movements
              )
          }
        },
      Duration("1h")
    )

  /** Handle a single arriving ev, that wants to charge
    *
    * @param ev
    *   The car to handle
    * @param pricesAtChargingStation
    *   Overview of prices per charging station
    * @param availableChargingPoints
    *   Overview of available charging points
    * @param maxDistance
    *   Maximum permissible distance between point of interest and charging
    *   station
    * @return
    *   An [[Future]] onto an [[Option]] of the target charging stations' UUID
    *   as well as information about the movement
    */
  private def handleArrivingEv(
      ev: ElectricVehicle,
      pricesAtChargingStation: Map[UUID, java.lang.Double],
      availableChargingPoints: Map[UUID, Integer],
      maxDistance: ComparableQuantity[Length]
  ): Future[Option[(UUID, Movement)]] = Future {
    chooseChargingStation(
      ev,
      pricesAtChargingStation,
      availableChargingPoints,
      MobilitySimulator.seed,
      maxDistance
    ).map { cs =>
      val availableChargingPointsAtStation: Integer =
        availableChargingPoints.getOrElse(cs, 0)
      if (availableChargingPointsAtStation > 0) {
        ev.setChargingAtSimona()
        ev.setChosenChargingStation(Some(cs))

        logger.debug(
          s"${ev.getId} starts charging at $cs."
        )

        Some((cs, Movement(cs, ev)))
      } else {
        logger.debug(
          s"${ev.getId} could not be charged at destination ${ev.getDestinationPoi} " +
            s"(${ev.getDestinationPoiType}) because all charging points " +
            s"at $cs were taken."
        )
        None
      }
    }.getOrElse {
      logger.debug(
        s"${ev.getId} parks but does not charge."
      )
      None
    }
  }

  /** Update and simulate EVs which are ending their parking at current time.
    * <p> First, the battery level is updated with the data returned from
    * SIMONA. Next, a new trip is simulated for the EV. <p> The set of electric
    * vehicles is updated for those EVs.
    *
    * @param currentTime
    *   current time
    * @param departedEvsFromSimona
    *   All EV objects were returned from SIMONA
    * @param categoricalLocation
    *   Meta-information to determine the next categorical location
    * @param drivingSpeed
    *   Meta-information to determine the next driving speed
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
    *   Maximum permissible distance to next charging hub
    */
  private def updateAndSimulateDepartedEvs(
      currentTime: ZonedDateTime,
      departedEvsFromSimona: SortedSet[ElectricVehicle],
      categoricalLocation: CategoricalLocation,
      drivingSpeed: DrivingSpeed,
      firstDepartureOfDay: FirstDepartureOfDay,
      lastTripOfDay: LastTripOfDay,
      parkingTime: ParkingTime,
      poiTransition: PoiTransition,
      tripDistance: TripDistance,
      thresholdChargingHubDistance: ComparableQuantity[Length]
  ): Unit = {

    val allDepartedEvs: Set[UUID] = electricVehicles
      .filter(_.getDepartureTime == currentTime)
      .map(filteredEv => filteredEv.getUuid)

    electricVehicles = electricVehicles.map(ev => {

      if (allDepartedEvs.contains(ev.getUuid)) {

        val departedEvFromSimona =
          departedEvsFromSimona.find(_.getUuid == ev.getUuid)

        val targetEv = departedEvFromSimona
          .map { departedEv =>
            /* Update the ev with the changed SoC from SIMONA */
            ev.copyWith(departedEv.getStoredEnergy)
          }
          .getOrElse(ev)

        simulateNextTrip(
          currentTime,
          targetEv,
          poisWithSizes,
          chargingHubTownIsPresent,
          chargingHubHighwayIsPresent,
          chargingStations,
          ioUtils,
          categoricalLocation,
          drivingSpeed,
          firstDepartureOfDay,
          lastTripOfDay,
          parkingTime,
          poiTransition,
          tripDistance,
          thresholdChargingHubDistance
        )
      } else ev
    })

  }

  /** Return the time in seconds until the earliest next event for one of the
    * EVs. The events are either departure or parking start.
    *
    * @param evs
    *   Set of all EVs
    * @param currentTime
    *   Current time to create a start value for foldLeft
    * @return
    *   Time until earliest next event for one of the EVs in the set of all EVs
    */
  private def getTimeUntilNextEvent(
      evs: Set[ElectricVehicle],
      currentTime: ZonedDateTime
  ): Long = {

    val nextEvent: ZonedDateTime = evs.foldLeft(currentTime.plusYears(1))(
      (earliestEvent: ZonedDateTime, ev: ElectricVehicle) => {
        val earlierEvent = if (ev.getParkingTimeStart.isAfter(currentTime)) {
          Seq(
            earliestEvent,
            ev.getParkingTimeStart,
            ev.getDepartureTime
          ).minOption.getOrElse(
            throw new IllegalArgumentException(
              "Unable to determine the earlier time"
            )
          )
        } else {
          Seq(earliestEvent, ev.getDepartureTime).minOption.getOrElse(
            throw new IllegalArgumentException(
              "Unable to determine the earlier time"
            )
          )
        }
        earlierEvent
      }
    )

    val timeUntilNextEvent: Long =
      currentTime.until(nextEvent, ChronoUnit.SECONDS)
    logger.debug(
      s"Next event in ${(timeUntilNextEvent / 60 / 60).toInt} hours " +
        s"and ${(timeUntilNextEvent / 60) % 60} minutes."
    )

    timeUntilNextEvent
  }
}

object MobilitySimulator
    extends ExtSimulation
    with ExtEvSimulation
    with ExtDataSimulation
    with LazyLogging {

  /** Class to describe a movement at a charging station. If it is a deparutre
    * or arrival is given by the context
    *
    * @param cs
    *   Unique identifier of the charging station
    * @param ev
    *   Ev model
    */
  final case class Movement(cs: UUID, ev: ElectricVehicle)

  private var simulator: Option[MobilitySimulator] = None

  /* TODO: All sets and lists in this simulation are ordered to reproduce the same results for
      different tests. This might be adapted for future applications.
   */

  /* random seed */
  val seed: Random = new scala.util.Random(6)

  private var evData: Option[ExtEvData] = None

  /** Set external EvAdapter during simulation setup */
  override def setExtEvData(evAdapter: ExtEvData): Unit = {
    this.evData = Some(evAdapter)
  }

  /** Activities to be performed every time the mobility simulator is triggered.
    *
    * @param tick
    *   Current time tick
    * @return
    *   Time tick (as ArrayList) when simulation should be triggered again
    */
  protected def doActivity(tick: Long): java.util.ArrayList[java.lang.Long] = {
    simulator
      .map(_.doActivity(tick))
      .getOrElse(
        throw UninitializedException("Simulation is not yet initialized")
      )
  }

  /** Initialize the simulation using the args received from SIMONA. <p> Sets
    * the paths for io with information received from SIMONA. Loads all
    * necessary data such as probabilities, EV models, etc. and creates all
    * objects such as EVs, POIs, and charging stations.
    */
  protected def initialize(): java.util.ArrayList[java.lang.Long] = {

    val availableEvData = evData.getOrElse(
      throw InitializationException(
        "Unable to access external ev data, although setup should have provided them."
      )
    )

    logger.info("Starting setup...")

    logger.debug("Parsing config")
    val config = ArgsParser
      .prepareConfig(getMainArgs)
      .getOrElse(
        throw InitializationException(
          s"Unable to parse config from given args '${getMainArgs
            .mkString("Array(", ", ", ")")}'. " +
            s"They have to contain at least 'config=<config_location>'."
        )
      )
    ConfigFailFast.check(config)

    /* Setup paths received from SIMONA */
    val pathsAndSources =
      PathsAndSources(config.mobsim.simulation.name, config.mobsim.input)

    val ioUtils = IoUtils(
      pathsAndSources.outputDir,
      "movements.csv",
      "evs.csv",
      "evcs.csv",
      "positions.csv",
      "pois.csv"
    )

    /* Load charging stations in the grid */
    val chargingStations = ChargingStation.loadChargingStationsWithPSDM(
      pathsAndSources.rawGridSource,
      pathsAndSources.systemParticipantSource
    )

    /* Load all POIs in the area */
    val maxDistanceFromPoi = Quantities.getQuantity(
      config.mobsim.simulation.location.maxDistanceToChargingStation,
      Units.METRE
    )
    val maxDistanceFromHomePoi = Quantities.getQuantity(
      config.mobsim.simulation.location.maxDistanceToHomeChargingStation,
      Units.METRE
    )
    val thresholdChargingHubDistance = Quantities.getQuantity(
      config.mobsim.simulation.location.chargingHubThresholdDistance,
      PowerSystemUnits.KILOMETRE
    )
    val pois = PoiUtils.loadPOIs(
      chargingStations,
      pathsAndSources.poiPath,
      maxDistanceFromPoi,
      maxDistanceFromHomePoi
    )
    ioUtils.writePois(pois)
    val poisWithSizes = PoiUtils.createPoiPdf(pois)

    val chargingHubTownIsPresent =
      pois.contains(CategoricalLocationDictionary.CHARGING_HUB_TOWN)
    val chargingHubHighwayIsPresent =
      pois.contains(CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY)

    /* Setup simulation start time */
    val startTime = TimeUtil.withDefaults
      .toZonedDateTime(config.mobsim.simulation.startDate)
      .withZoneSameInstant(ZoneId.of("UTC"))

    logger.debug("Loading probabilities for first departure of day")
    val firstDepartureOfDay = FirstDepartureFactory.getFromFile(
      pathsAndSources.firstDepartureOfDayPath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for first departure of day from path.",
          exception
        )
      case Success(value) => value
    }

    /* Initialize all EV objects in the area */
    val evModelPdf =
      EvTypeInput.getEvInputModelsWithProbabilities(
        pathsAndSources.evInputModelPath,
        pathsAndSources.evSegmentPath
      )
    val numberOfEvsInArea = config.mobsim.simulation.numberOfEv
    val targetShareOfHomeCharging =
      config.mobsim.simulation.targetHomeChargingShare
    val start = System.currentTimeMillis()
    logger.info(
      s"Creating $numberOfEvsInArea evs with a targeted home charging share of ${"%.2f"
        .format(targetShareOfHomeCharging * 100)} %."
    )
    val evs = ElectricVehicle.createEvs(
      numberOfEvsInArea,
      poisWithSizes
        .getOrElse(
          CategoricalLocationDictionary.HOME,
          throw InitializationException(
            "Unable to obtain the probability density function for home POI."
          )
        )
        .pdf,
      poisWithSizes.getOrElse(
        CategoricalLocationDictionary.WORK,
        throw InitializationException(
          "Unable to obtain the probability density function for work POI."
        )
      ),
      chargingStations,
      startTime,
      targetShareOfHomeCharging,
      evModelPdf,
      firstDepartureOfDay
    )
    ioUtils.writeEvs(evs)

    logger.info(
      s"Created ${evs.size} EVs in ${"%.2f"
        .format((System.currentTimeMillis() - start) / 1000d)} s, of which ${evs
        .count(_.isChargingAtHomePossible)} can charge at home."
    )

    /* Calculate and print total number of charging points at charging stations */
    val numberOfChargingPointsInArea = chargingStations.foldLeft((0, 0, 0, 0))(
      (
          totalNumberOfChargingPoints: (Int, Int, Int, Int),
          cs: ChargingStation
      ) => {
        (
          totalNumberOfChargingPoints._1 + cs.getChargingPoints,
          if (cs.getEvcsLocationType == EvcsLocationType.HOME)
            totalNumberOfChargingPoints._2 + cs.getChargingPoints
          else totalNumberOfChargingPoints._2,
          if (
            cs.getEvcsLocationType == EvcsLocationType.WORK || cs.getEvcsLocationType == EvcsLocationType.STREET
            || cs.getEvcsLocationType == EvcsLocationType.CUSTOMER_PARKING
          )
            totalNumberOfChargingPoints._3 + cs.getChargingPoints
          else totalNumberOfChargingPoints._3,
          if (
            cs.getEvcsLocationType == EvcsLocationType.CHARGING_HUB_TOWN
            || cs.getEvcsLocationType == EvcsLocationType.CHARGING_HUB_HIGHWAY
          )
            totalNumberOfChargingPoints._4 + cs.getChargingPoints
          else totalNumberOfChargingPoints._4
        )
      }
    )
    logger.info(
      s"Numbers of charging points in the area:  " +
        s"Total: ${numberOfChargingPointsInArea._1}, " +
        s"Home: ${numberOfChargingPointsInArea._2}, " +
        s"Public: ${numberOfChargingPointsInArea._3}, " +
        s"Charging hubs: ${numberOfChargingPointsInArea._4}. Consider that not all charging stations are " +
        s"necessarily within reach of the POIs!"
    )

    val categoricalLocation = CategoricalLocationFactory.getFromFile(
      pathsAndSources.categoricalLocationPath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get categorical location probabilities from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for categorical locations")

    val drivingSpeed = DrivingSpeedFactory.getFromFile(
      pathsAndSources.drivingSpeedPath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get driving speed parameters from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for driving speed")

    val lastTripOfDay = LastTripFactory.getFromFile(
      pathsAndSources.lastTripPath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get last trip probabilities from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for last trip of day")

    val parkingTime = ParkingTimeFactory.getFromFile(
      pathsAndSources.parkingTimePath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for parking time from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for parking time")

    val poiTransition = PoiTransitionFactory.getFromFile(
      pathsAndSources.poiTransitionPath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for poi type transitions from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for poi transition")

    val tripDistance = TripDistanceFactory.getFromFile(
      pathsAndSources.tripDistancePath,
      config.mobsim.input.mobility.source.colSep
    ) match {
      case Failure(exception) =>
        throw SourceException(
          "Unable to get probabilities for trip distance transitions from path.",
          exception
        )
      case Success(value) => value
    }
    logger.debug("Done loading probabilities for trip distance")

    val mobSim = new MobilitySimulator(
      availableEvData,
      chargingStations,
      poisWithSizes,
      startTime,
      evs,
      chargingHubTownIsPresent,
      chargingHubHighwayIsPresent,
      ioUtils,
      categoricalLocation,
      drivingSpeed,
      firstDepartureOfDay,
      lastTripOfDay,
      parkingTime,
      poiTransition,
      tripDistance,
      maxDistanceFromPoi,
      thresholdChargingHubDistance
    )
    simulator = Some(mobSim)

    logger.info("Finished setup!")

    mobSim.doActivity(-1L)
  }
}
