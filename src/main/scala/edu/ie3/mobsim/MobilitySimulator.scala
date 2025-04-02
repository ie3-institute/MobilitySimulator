/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input.EvInputSource
import edu.ie3.mobsim.config.{ArgsParser, ConfigFailFast}
import edu.ie3.mobsim.exceptions.{
  InitializationException,
  UninitializedException,
}
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.{HomePoiMapping, PoiUtils, PointOfInterest}
import edu.ie3.mobsim.io.probabilities._
import edu.ie3.mobsim.model.ChargingStation.chooseChargingStation
import edu.ie3.mobsim.model.TripSimulation.simulateNextTrip
import edu.ie3.mobsim.model.builder.{
  EvBuilderFromEvInput,
  EvBuilderFromEvInputWithEvcsMapping,
  EvBuilderFromRandomModel,
}
import edu.ie3.mobsim.model.{
  ChargingStation,
  ElectricVehicle,
  EvMovement,
  EvType,
}
import edu.ie3.mobsim.utils.{IoUtils, PathsAndSources}
import edu.ie3.simona.api.data.ExtDataConnection
import edu.ie3.simona.api.data.ev.ExtEvDataConnection
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.simona.api.simulation.ExtSimulation
import edu.ie3.util.TimeUtil
import squants.Length
import squants.space.{Kilometers, Meters}

import java.time.temporal.ChronoUnit
import java.time.{ZoneId, ZonedDateTime}
import java.util
import java.util.{Optional, UUID}
import scala.collection.parallel.CollectionConverters._
import scala.jdk.CollectionConverters._
import scala.jdk.OptionConverters._
import scala.util.Random

final class MobilitySimulator(
    evDataConnection: ExtEvDataConnection,
    chargingStations: Seq[ChargingStation],
    poisWithSizes: Map[
      CategoricalLocationDictionary.Value,
      ProbabilityDensityFunction[PointOfInterest],
    ],
    startTime: ZonedDateTime,
    var electricVehicles: Seq[ElectricVehicle],
    chargingHubTownIsPresent: Boolean,
    chargingHubHighwayIsPresent: Boolean,
    ioUtils: IoUtils,
    tripProbabilities: TripProbabilities,
    maxDistanceFromPoi: Length,
    thresholdChargingHubDistance: Length,
    round15: Boolean,
) extends LazyLogging {

  /** Activities to be performed every time the mobility simulator is triggered.
    *
    * @param tick
    *   Current time tick
    * @return
    *   Next time tick when simulation should be triggered again
    */
  private def doActivity(tick: Long): Optional[java.lang.Long] = {
    /* Update current time */
    val currentTime = startTime.plusSeconds(tick)

    logger.info(
      s"Simulation triggered with tick: $tick -- Current time: $currentTime"
    )

    /* Receive available charging points of evcs from SIMONA and converting them to scala values */
    val availableChargingPoints = evDataConnection
      .requestAvailablePublicEvcs()
      .asScala
      .view
      .mapValues(_.toInt)
      .toMap

    /* Receive current prices for public evcs situation and converting them to scala values */
    val currentPricesAtChargingStations = evDataConnection
      .requestCurrentPrices()
      .asScala
      .view
      .mapValues(_.toDouble)
      .toMap

    /* Send EV movements to SIMONA and receive charged EVs that ended parking */
    exchangeEvMovementsWithSimona(
      currentTime,
      tick,
      availableChargingPoints,
      currentPricesAtChargingStations,
      maxDistanceFromPoi,
    )

    /* Get time until next event for one of the EVs and return corresponding tick to SIMONA */
    val timeUntilNextEvent = MobilitySimulator.getTimeUntilNextEvent(
      electricVehicles,
      currentTime,
    )

    timeUntilNextEvent.foreach { nextEvent =>
      logger.debug(
        s"Next event in ${(nextEvent / 60 / 60).toInt} hours " +
          s"and ${(nextEvent / 60) % 60} minutes."
      )
    }

    timeUntilNextEvent.map { nextEvent =>
      long2Long(tick + nextEvent)
    }.toJava
  }

  /** Hand over EVs to SIMONA which arrive at their destination POI or depart
    * from their POI at current time, but only if there is a charging point
    * available at the charging station. Otherwise, the EV is not sent to
    * SIMONA. <p> Get updated EV models for all EVs that depart from a charging
    * station and are returned from SIMONA.
    *
    * @param currentTime
    *   current time of the simulation
    * @param tick
    *   the current tick
    * @param availableChargingPoints
    *   Map with currently available charging points at the evcs
    * @param currentPricesAtChargingStations
    *   Currently known prices per charging station
    * @param maxDistance
    *   Maximum permissible distance between a POI and a charging station
    */
  private def exchangeEvMovementsWithSimona(
      currentTime: ZonedDateTime,
      tick: Long,
      availableChargingPoints: Map[UUID, Int],
      currentPricesAtChargingStations: Map[UUID, Double],
      maxDistance: Length,
  ) = {

    val departingEvs = filterDepartingEvs(electricVehicles, currentTime)

    /* !!! Attention - Departing and parking evs have to be treated sequentially !!!
     * The departing cars offer additional free lots to the parking cars */
    val (departures, updatedChargingPoints) =
      handleDepartures(departingEvs, availableChargingPoints)

    val departedEvs = evDataConnection
      .requestDepartingEvs(
        EvMovement.buildMovementsUuidMap(departures)
      )
      .asScala
      .map {
        case ev: ElectricVehicle => ev
        case unexpected =>
          throw new IllegalArgumentException(
            s"Got unexpected EvModel type ${unexpected.getClass}"
          )
      }
      .toSeq
    logger.debug("Requested {} departing EVs from SIMONA", departures.size)

    val parkingEvs = filterParkingEvs(electricVehicles, currentTime)

    // Add EVs that start parking now to movements and assign to Evcs UUID
    val arrivals = handleParkingEvs(
      parkingEvs,
      currentPricesAtChargingStations,
      updatedChargingPoints,
      maxDistance,
    )

    // Update and simulate all EVs that were returned from SIMONA
    updateAndSimulateDepartedEvs(
      currentTime,
      departedEvs,
      tripProbabilities,
      thresholdChargingHubDistance,
      round15,
    )

    // Finally, send arrivals to SIMONA.
    // Next event could be departure-only, in which case empty arrivals will be sent.
    val timeUntilNextEvent =
      MobilitySimulator
        .getTimeUntilNextEvent(electricVehicles, currentTime)
        .map(_ + tick)
    evDataConnection.provideArrivingEvs(
      EvMovement.buildMovementsMap(arrivals),
      timeUntilNextEvent.map(long2Long).toJava,
    )
    logger.debug("Sent {} arriving EVs to SIMONA", arrivals.size)
  }

  /** Determine the set of EVs that depart at this tick and have been sent to
    * SIMONA.
    *
    * @param evs
    *   Collection of known EVs
    * @param currentTime
    *   The current simulation time
    * @return
    *   Departing EVs
    */
  private def filterDepartingEvs(
      evs: Seq[ElectricVehicle],
      currentTime: ZonedDateTime,
  ): Seq[ElectricVehicle] = {
    /* Relevant are only cars, that depart AND that are charging at a suitable charging station in SIMONA */
    val isDeparting = (ev: ElectricVehicle) =>
      ev.departureTime == currentTime && ev.chargingAtSimona && ev.chosenChargingStation.isDefined
    evs.par.filter(isDeparting).seq
  }

  /** Determine the set of EVs that start parking at this tick
    *
    * @param evs
    *   Collection of known EVs
    * @param currentTime
    *   The current simulation time
    * @return
    *   Parking EVs
    */
  private def filterParkingEvs(
      evs: Seq[ElectricVehicle],
      currentTime: ZonedDateTime,
  ): Seq[ElectricVehicle] = {
    val isParking = (ev: ElectricVehicle) => ev.parkingTimeStart == currentTime
    evs.par.filter(isParking).seq
  }

  /** Handle all ev departures
    *
    * @param evs
    *   Collection of departing evs
    * @param availableChargingPoints
    *   Overview of free charging points per charging station
    * @return
    *   An updated overview of free charging lots per charging station
    */
  private def handleDepartures(
      evs: Seq[ElectricVehicle],
      availableChargingPoints: Map[UUID, Int],
  ): (Seq[EvMovement], Map[UUID, Int]) = {
    val (additionallyFreeChargingPoints, departures) =
      handleDepartingEvs(evs)

    (
      departures,
      updateFreeLots(availableChargingPoints, additionallyFreeChargingPoints),
    )
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
      evs: Seq[ElectricVehicle]
  ): (Map[UUID, Int], Seq[EvMovement]) =
    evs.par.flatMap(handleDepartingEv) match {
      case movements =>
        updateElectricVehicles(movements.seq)

        (
          movements
            .map(_.cs)
            .groupBy(identity)
            .map { case (uuid, uuids) =>
              uuid -> uuids.size
            }
            .seq,
          movements.seq,
        )
    }

  /** Handle a single departing ev. Empty information are handed back, if the
    * car either does not charge in SIMONA or there is no target charging
    * station defined.
    *
    * @param ev
    *   The car to handle
    * @return
    *   An [[Option]] of the target charging stations' UUID as well as
    *   information about the movement
    */
  private def handleDepartingEv(
      ev: ElectricVehicle
  ): Option[EvMovement] =
    /* Determine the charging station, the car currently is connected to */
    ev.chosenChargingStation match {
      case Some(cs) =>
        val updatedEv =
          ev.removeChargingAtSimona().setChosenChargingStation(None)

        Some(EvMovement(cs, updatedEv))
      case None =>
        logger.warn(
          s"Ev '$ev' is meant to charge in SIMONA, but no charging station is set."
        )
        None
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
      freeLots: Map[UUID, Int],
      change: Map[UUID, Int],
  ): Map[UUID, Int] =
    freeLots.map { case (uuid, freeLots) =>
      val lotDiff = change.getOrElse(uuid, 0)
      val newLots = freeLots + lotDiff
      uuid -> newLots
    } ++ change
      .filter { case (uuid, _) =>
        !freeLots.keys.toSeq.contains(uuid)
      }
      .map { case (uuid, lotDifference) =>
        uuid -> lotDifference
      }

  /** Handle parking evs. Based on given surrounding information, a target
    * charging station is sampled. The movements are tracked and handed back.
    *
    * @param parkingEvs
    *   Collection of parking EVs
    * @param pricesAtChargingStation
    *   Prices at known charging stations
    * @param availableChargingPoints
    *   Available charging points per charging station
    * @param maxDistance
    *   Maximum permissible distance between an point of interest and a
    *   candidate charging station
    * @return
    *   A collection of movements
    */
  private def handleParkingEvs(
      parkingEvs: Seq[ElectricVehicle],
      pricesAtChargingStation: Map[UUID, Double],
      availableChargingPoints: Map[UUID, Int],
      maxDistance: Length,
  ): Seq[EvMovement] =
    parkingEvs.foldLeft((availableChargingPoints, Seq.empty[EvMovement])) {
      case ((updatedAvailableChargingPoints, movements), ev) =>
        /* Lets the EV choose whether and at which charging station it wants to charge */
        handleArrivingEv(
          ev,
          pricesAtChargingStation,
          updatedAvailableChargingPoints,
          maxDistance,
        ).flatMap {
          // in case of a successful start of charging,
          // remove the charging point from available points
          // so that it won't be chosen more than once
          case movement @ EvMovement(cs, _) =>
            updatedAvailableChargingPoints.get(cs).map { count =>
              (
                updatedAvailableChargingPoints + (cs -> (count - 1)),
                movements :+ movement,
              )
            }
        }.getOrElse((updatedAvailableChargingPoints, movements))
    } match {
      case (_, arrivals) =>
        updateElectricVehicles(arrivals)
        arrivals
    }

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
    *   An [[Option]] of the movement towards a charging station, if one was
    *   made
    */
  private def handleArrivingEv(
      ev: ElectricVehicle,
      pricesAtChargingStation: Map[UUID, Double],
      availableChargingPoints: Map[UUID, Int],
      maxDistance: Length,
  ): Option[EvMovement] = {
    val minParkingTimeForCharging = 15
    val staysLongEnough = ev.parkingTimeStart
      .until(ev.departureTime, ChronoUnit.MINUTES) >= minParkingTimeForCharging
    if (staysLongEnough) {
      val (chosenCsOpt, updatedEvOpt) = chooseChargingStation(
        ev,
        pricesAtChargingStation,
        availableChargingPoints,
        MobilitySimulator.seed,
        maxDistance,
      )

      chosenCsOpt
        .map { cs =>
          val availableChargingPointsAtStation =
            availableChargingPoints.getOrElse(cs, 0)
          if (availableChargingPointsAtStation > 0) {

            val updatedEv = updatedEvOpt
              .getOrElse(ev)
              .setChargingAtSimona()
              .setChosenChargingStation(Some(cs))

            Some(EvMovement(cs, updatedEv))
          } else {
            logger.debug(
              s"${ev.getId} could not be charged at destination ${ev.destinationPoi} " +
                s"(${ev.destinationPoiType}) because all charging points " +
                s"at $cs were taken."
            )
            None
          }
        }
        .getOrElse {
          logger.debug(
            s"${ev.getId} parks but does not charge."
          )
          None
        }
    } else {
      logger.debug(
        s"s${ev.id} parks but does not charge, since parking time is below $minParkingTimeForCharging minutes."
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
    * @param tripProbabilities
    *   Probabilities to generate new trips
    * @param thresholdChargingHubDistance
    *   Maximum permissible distance to next charging hub
    */
  private def updateAndSimulateDepartedEvs(
      currentTime: ZonedDateTime,
      departedEvsFromSimona: Seq[ElectricVehicle],
      tripProbabilities: TripProbabilities,
      thresholdChargingHubDistance: Length,
      round15: Boolean,
  ): Unit = {

    // here a set is useful for fast containment checking
    val allDepartedEvs: Set[UUID] = electricVehicles
      .filter(_.departureTime == currentTime)
      .map(filteredEv => filteredEv.getUuid)
      .toSet

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
          tripProbabilities,
          thresholdChargingHubDistance,
          round15,
        )
      } else ev
    })
  }

  private def updateElectricVehicles(movements: Seq[EvMovement]): Unit = {
    val movementMap = movements.map { case EvMovement(_, ev) =>
      ev.uuid -> ev
    }.toMap

    // updates all ev in electricVehicles that are departing or arriving
    electricVehicles = electricVehicles.map { ev =>
      movementMap.getOrElse(ev.uuid, ev)
    }
  }
}

object MobilitySimulator
    extends ExtSimulation("MobilitySimulator")
    with LazyLogging {

  private var simulator: Option[MobilitySimulator] = None

  /* TODO: All sets and lists in this simulation are ordered to reproduce the same results for
      different tests. This might be adapted for future applications.
   */

  /* random seed */
  val seed: Random = new scala.util.Random(6)

  private val evDataConnection: ExtEvDataConnection = new ExtEvDataConnection()

  override def getDataConnections: util.Set[ExtDataConnection] =
    util.Set.of(evDataConnection)

  /** Activities to be performed every time the mobility simulator is triggered.
    *
    * @param tick
    *   Current time tick
    * @return
    *   Next time tick when simulation should be triggered again
    */
  override protected def doActivity(tick: Long): Optional[java.lang.Long] = {
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
  override protected def initialize(): java.lang.Long = {

    val initTick = -1L

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
      PathsAndSources(
        config.mobsim.simulation.name,
        config.mobsim.input,
        config.mobsim.output.outputDir,
      )

    val ioUtils = IoUtils(
      pathsAndSources.outputDir,
      "movements.csv",
      "evs.csv",
      "positions.csv",
      "pois.csv",
      config.mobsim.output.writeMovements,
    )

    /* Load charging stations in the grid */
    val chargingStations = ChargingStation.loadChargingStationsWithPSDM(
      pathsAndSources.rawGridSource,
      pathsAndSources.systemParticipantSource,
    )

    /* Load all POIs in the area */
    val maxDistanceFromPoi = Meters(
      config.mobsim.simulation.location.maxDistanceToChargingStation
    )
    val maxDistanceFromHomePoi = Meters(
      config.mobsim.simulation.location.maxDistanceToHomeChargingStation
    )
    val thresholdChargingHubDistance = Kilometers(
      config.mobsim.simulation.location.chargingHubThresholdDistance
    )

    /* in case we defined an explicit home poi to evcs mapping we don't need to assign
    nearest charging stations to home pois by their distance */
    val assignHomeNearestChargingStations =
      config.mobsim.input.evInputSource match {
        case Some(EvInputSource(homePoiMapping, _))
            if homePoiMapping.isDefined =>
          false
        case _ => true
      }

    val pois = PoiUtils.loadPOIs(
      chargingStations,
      pathsAndSources.poiPath,
      maxDistanceFromPoi,
      maxDistanceFromHomePoi,
      assignHomeNearestChargingStations,
    )
    val poisWithSizes = PoiUtils.createPoiPdf(pois)

    val chargingHubTownIsPresent =
      pois.contains(CategoricalLocationDictionary.CHARGING_HUB_TOWN)
    val chargingHubHighwayIsPresent =
      pois.contains(CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY)

    /* Setup simulation start time */
    val startTime = TimeUtil.withDefaults
      .toZonedDateTime(config.mobsim.simulation.startDate)
      .withZoneSameInstant(ZoneId.of("UTC"))

    /* Initialize all EV objects in the area */
    val evModelPdf =
      EvType.getEvInputModelsWithProbabilities(
        pathsAndSources.evInputModelPath,
        pathsAndSources.evSegmentPath,
      )
    val numberOfEvsInArea = config.mobsim.simulation.numberOfEv
    val targetShareOfHomeCharging =
      config.mobsim.simulation.targetHomeChargingShare
    val start = System.currentTimeMillis()
    logger.info(
      s"Creating evs with a targeted home charging share of ${"%.2f"
          .format(targetShareOfHomeCharging * 100)} %."
    )

    val tripProbabilities = TripProbabilities.read(
      pathsAndSources,
      config.mobsim.input.mobility.source.colSep,
      config.mobsim.simulation.averageCarUsage,
      config.mobsim.simulation.round15,
    )

    val homePOIsWithSizes = poisWithSizes
      .getOrElse(
        CategoricalLocationDictionary.HOME,
        throw InitializationException(
          "Unable to obtain the probability density function for home POI."
        ),
      )
      .pdf

    val workPoiPdf = poisWithSizes.getOrElse(
      CategoricalLocationDictionary.WORK,
      throw InitializationException(
        "Unable to obtain the probability density function for work POI."
      ),
    )

    val (evs, evcsDirectHomePoiMapping) =
      config.mobsim.input.evInputSource match {
        case Some(csvParams) =>
          val evInputs =
            IoUtils.readEvInputs(
              csvParams.source
            )

          csvParams.homePoiMapping match {
            case Some(mappingSource) =>
              val mappingEntries = HomePoiMapping.readPoiMapping(mappingSource)
              val (ev2poi, poi2evcs) = HomePoiMapping.getMaps(mappingEntries)

              (
                EvBuilderFromEvInputWithEvcsMapping.build(
                  evInputs,
                  homePOIsWithSizes,
                  workPoiPdf,
                  chargingStations,
                  startTime,
                  tripProbabilities.firstDepartureOfDay,
                  ev2poi,
                  poi2evcs,
                ),
                poi2evcs,
              )

            case None =>
              (
                EvBuilderFromEvInput.build(
                  evInputs,
                  homePOIsWithSizes,
                  workPoiPdf,
                  chargingStations,
                  startTime,
                  targetShareOfHomeCharging,
                  tripProbabilities.firstDepartureOfDay,
                ),
                Map.empty[UUID, UUID],
              )
          }

        case None =>
          (
            EvBuilderFromRandomModel.build(
              numberOfEvsInArea,
              homePOIsWithSizes,
              workPoiPdf,
              chargingStations,
              startTime,
              targetShareOfHomeCharging,
              evModelPdf,
              tripProbabilities.firstDepartureOfDay,
            ),
            Map.empty[UUID, UUID],
          )
      }

    ioUtils.writePois(pois, evcsDirectHomePoiMapping)

    ioUtils.writeEvs(evs)

    logger.info(
      s"Created ${evs.size} EVs in ${"%.2f"
          .format((System.currentTimeMillis() - start) / 1000d)} s, of which ${evs
          .count(_.chargingAtHomePossible)} can charge at home."
    )

    /* Calculate and print total number of charging points at charging stations */
    val numberOfChargingPointsInArea = chargingStations.foldLeft((0, 0, 0, 0))(
      (
          totalNumberOfChargingPoints: (Int, Int, Int, Int),
          cs: ChargingStation,
      ) => {
        (
          totalNumberOfChargingPoints._1 + cs.chargingPoints,
          if (cs.evcsLocationType == EvcsLocationType.HOME)
            totalNumberOfChargingPoints._2 + cs.chargingPoints
          else totalNumberOfChargingPoints._2,
          if (
            cs.evcsLocationType == EvcsLocationType.WORK || cs.evcsLocationType == EvcsLocationType.STREET
            || cs.evcsLocationType == EvcsLocationType.CUSTOMER_PARKING
          )
            totalNumberOfChargingPoints._3 + cs.chargingPoints
          else totalNumberOfChargingPoints._3,
          if (
            cs.evcsLocationType == EvcsLocationType.CHARGING_HUB_TOWN
            || cs.evcsLocationType == EvcsLocationType.CHARGING_HUB_HIGHWAY
          )
            totalNumberOfChargingPoints._4 + cs.chargingPoints
          else totalNumberOfChargingPoints._4,
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

    val mobSim = new MobilitySimulator(
      evDataConnection,
      chargingStations,
      poisWithSizes,
      startTime,
      evs,
      chargingHubTownIsPresent,
      chargingHubHighwayIsPresent,
      ioUtils,
      tripProbabilities,
      maxDistanceFromPoi,
      thresholdChargingHubDistance,
      config.mobsim.simulation.round15,
    )
    simulator = Some(mobSim)

    val currentTime = startTime.plusSeconds(initTick)
    val timeUntilFirstEvent =
      getTimeUntilNextEvent(evs, currentTime).getOrElse(
        throw InitializationException(
          "No first event determined!"
        )
      )
    val firstEventTick = initTick + timeUntilFirstEvent

    // Also provide first tick to the data service
    evDataConnection.provideArrivingEvs(
      Map.empty[UUID, java.util.List[EvModel]].asJava,
      Some(long2Long(firstEventTick)).toJava,
    )

    logger.info("Finished setup!")

    firstEventTick
  }

  /** Return the time in seconds until the earliest next EV is departing or
    * parking (and possibly charging in SIMONA).
    *
    * EVs that are departing, but have not been sent to SIMONA, are included as
    * well, because they have to be re-simulated as well.
    *
    * If there are parked EVs at the next event, we do not know at this point
    * whether there will be arrivals as well, since EVs can park without
    * charging. This means that it is possible there will be no arrivals at the
    * returned tick.
    *
    * @param evs
    *   Collection of all EVs
    * @param currentTime
    *   Current simulation time
    * @return
    *   Time until earliest next parking event for one of the EVs in the set of
    *   all EVs
    */
  private def getTimeUntilNextEvent(
      evs: Seq[ElectricVehicle],
      currentTime: ZonedDateTime,
  ): Option[Long] =
    evs
      .foldLeft[Option[ZonedDateTime]](None)(
        (earliestEvent, ev: ElectricVehicle) =>
          if (ev.parkingTimeStart.isAfter(currentTime))
            Seq(
              earliestEvent,
              Some(ev.parkingTimeStart),
            ).flatten.minOption
          else if (ev.departureTime.isAfter(currentTime))
            Seq(earliestEvent, Some(ev.departureTime)).flatten.minOption
          else
            earliestEvent
      )
      .map { nextEvent =>
        currentTime.until(nextEvent, ChronoUnit.SECONDS)
      }

}
