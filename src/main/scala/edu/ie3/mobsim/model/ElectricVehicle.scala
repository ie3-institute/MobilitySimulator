/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.EvInput
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{
  FirstDepartureOfDay,
  ProbabilityDensityFunction
}
import edu.ie3.mobsim.utils.utils.toTick
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import java.util.UUID
import javax.measure.quantity.{Energy, Length, Power}
import scala.collection.immutable.Queue
import scala.util.{Failure, Success, Try}

/** Class to denote electric vehicle and its current trip.
  *
  * @param simulationStart
  *   start time of simulation
  * @param uuid
  *   its uuid
  * @param id
  *   its id
  * @param evType
  *   type information of car
  * @param homePoi
  *   the vehicles home poi
  * @param workPoi
  *   the vehicles work poi
  * @param storedEnergy
  *   current soc
  * @param destinationPoi
  *   destination POI of current trip
  * @param parkingTimeStart
  *   parking start time of current trip
  * @param departureTime
  *   departure time from destination
  * @param chargingAtHomePossible
  *   whether car can charge at home
  * @param chosenChargingStation
  *   the chosen charging station it wants to charge at
  * @param chargingAtSimona
  *   whether the car will charge at SIMONA
  * @param finalDestinationPoi
  *   stores final destination if making a trip to charging hub
  * @param finalDestinationPoiType
  *   stores the final destination poi type if making a trip to charging hub
  * @param remainingDistanceAfterChargingHub
  *   distance remaining when departing from charging hub
  * @param chargingPricesMemory
  *   the charging prices at charging stations
  */
final case class ElectricVehicle(
    simulationStart: ZonedDateTime,
    uuid: UUID,
    id: String,
    evType: EvType,
    homePoi: PointOfInterest,
    workPoi: PointOfInterest,
    storedEnergy: ComparableQuantity[Energy],
    destinationPoi: PointOfInterest,
    destinationPoiType: PoiTypeDictionary.Value,
    parkingTimeStart: ZonedDateTime,
    departureTime: ZonedDateTime,
    chargingAtHomePossible: Boolean,
    chosenChargingStation: Option[UUID],
    chargingAtSimona: Boolean,
    finalDestinationPoi: Option[PointOfInterest],
    finalDestinationPoiType: Option[PoiTypeDictionary.Value],
    remainingDistanceAfterChargingHub: Option[
      ComparableQuantity[Length]
    ],
    chargingPricesMemory: Queue[Double]
) extends EvModel
    with Ordered[ElectricVehicle] {

  def getUuid: UUID = uuid

  def getId: String = id

  def getEStorage: ComparableQuantity[Energy] = evType.capacity

  def getSRatedAC: ComparableQuantity[Power] = evType.acPower

  def getSRatedDC: ComparableQuantity[Power] = evType.dcPower

  def getStoredEnergy: ComparableQuantity[Energy] = storedEnergy

  def getDepartureTick: java.lang.Long = toTick(simulationStart, departureTime)

  /** @param storedEnergy
    *   the new stored energy
    * @return
    *   a copy of this ElectricVehicle with given new stored energy / soc
    */
  def copyWith(storedEnergy: ComparableQuantity[Energy]): ElectricVehicle =
    copy(storedEnergy = storedEnergy)

  /** @param storedEnergy
    *   the new stored energy
    * @return
    *   a copy of this ElectricVehicle with given new values
    */
  def copyWith(
      storedEnergy: ComparableQuantity[Energy],
      destinationPoi: PointOfInterest,
      destinationPoiType: PoiTypeDictionary.Value,
      parkingTimeStart: ZonedDateTime,
      departureTime: ZonedDateTime
  ): ElectricVehicle =
    copy(
      storedEnergy = storedEnergy,
      destinationPoi = destinationPoi,
      destinationPoiType = destinationPoiType,
      parkingTimeStart = parkingTimeStart,
      departureTime = departureTime
    )

  def setChosenChargingStation(
      chargingStation: Option[UUID]
  ): ElectricVehicle = {
    copy(chosenChargingStation = chargingStation)
  }

  def setChargingAtSimona(): ElectricVehicle = {
    copy(chargingAtSimona = true)
  }

  def removeChargingAtSimona(): ElectricVehicle = {
    copy(chargingAtSimona = false)
  }

  def resetFinalDestination(): ElectricVehicle = {
    copy(finalDestinationPoi = None, finalDestinationPoiType = None)
  }

  def setFinalDestination(
      destinationPoi: PointOfInterest,
      destinationPoiType: PoiTypeDictionary.Value
  ): ElectricVehicle = {
    copy(
      finalDestinationPoi = Some(destinationPoi),
      finalDestinationPoiType = Some(destinationPoiType)
    )
  }

  def setRemainingDistanceAfterChargingHub(
      remainingDistance: Option[ComparableQuantity[Length]]
  ): ElectricVehicle = {
    copy(remainingDistanceAfterChargingHub = remainingDistance)
  }

  def updateChargingPricesMemory(
      newPrices: Queue[Double]
  ): ElectricVehicle = {
    val newPriceQueue = (chargingPricesMemory ++ newPrices).takeRight(20)

    copy(chargingPricesMemory = newPriceQueue)
  }

  def compare(that: ElectricVehicle): Int = {
    if (this.id == that.id) 0
    else if (this.id > that.id) 1
    else -1
  }

  override def toString: String =
    s"EV(id=$id, eStorage=${evType.capacity}, storedEnergy=$storedEnergy, AC=${evType.acPower}, DC=${evType.dcPower}, departureTime=$departureTime)"

}

case object ElectricVehicle extends LazyLogging {

  def createEvsFromEvInput(
      electricVehicles: Seq[EvInput],
      homePOIsWithSizes: Map[PointOfInterest, Double],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      chargingStations: Seq[ChargingStation],
      startTime: ZonedDateTime,
      targetSharePrivateCharging: Double,
      firstDepartureOfDay: FirstDepartureOfDay
  ): Seq[ElectricVehicle] = {
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

    val evs = initialHomeChargingCars.toSeq ++ additionalCars
    logger.info(s"Created ${evs.size} EVs from EvInputs during setup.")
    evs
  }

  /** Initially create all EV objects for the simulation. The EV objects are
    * saved in a mutable list. For the parametrization of the EVs, the loaded
    * probabilities are used.
    */
  def createEvs(
      numberOfEvsInArea: Int,
      homePOIsWithSizes: Map[PointOfInterest, Double],
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      chargingStations: Seq[ChargingStation],
      startTime: ZonedDateTime,
      targetSharePrivateCharging: Double,
      evModelPdf: ProbabilityDensityFunction[EvType],
      firstDepartureOfDay: FirstDepartureOfDay
  ): Seq[ElectricVehicle] = {
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

    val evs = initialHomeChargingCars.toSeq ++ additionalCars
    logger.info(s"Created ${evs.size} EVs by model sampling during setup.")
    evs
  }

  /** Determine probability density functions for home POI separated by the
    * possible to charge at home or not
    *
    * @param homePoisWithSizes
    *   Mapping from available home POI to it's "size"
    * @param chargingStations
    *   Collection of available charging stations
    * @return
    */
  private def determineHomePoiPdf(
      homePoisWithSizes: Map[PointOfInterest, Double],
      chargingStations: Seq[ChargingStation]
  ): (
      ProbabilityDensityFunction[PointOfInterest],
      ProbabilityDensityFunction[PointOfInterest]
  ) = {
    /* Map all home POIs to the possibility to charge at home */
    val homePoiToHomeChargingOption = homePoisWithSizes.keys.map { poi =>
      poi -> chargingAtHomePossibleForHomePOI(poi, chargingStations)
    }.toMap

    /* Create probability density functions separately for home POI with and without charging option */
    val homePoiPdfWithHomeCharging = Try {
      ProbabilityDensityFunction(
        homePoisWithSizes.filter { case (poi, _) =>
          homePoiToHomeChargingOption.getOrElse(poi, false)
        }
      )
    } match {
      case Failure(exception) =>
        throw InitializationException(
          "Unable to build pdf for home POI with home charging option",
          exception
        )
      case Success(value) => value
    }
    val homePoiPdfWithoutHomeCharging = Try {
      ProbabilityDensityFunction(
        homePoisWithSizes.filter { case (poi, _) =>
          !homePoiToHomeChargingOption.getOrElse(poi, true)
        }
      )
    } match {
      case Failure(exception) =>
        throw InitializationException(
          "Unable to build pdf for home POI without home charging option",
          exception
        )
      case Success(value) => value
    }

    (homePoiPdfWithHomeCharging, homePoiPdfWithoutHomeCharging)
  }

  /** Check for a POI whether it has a charging station of EvcsLocationType
    * "home" nearby, hence a private home charging option.
    *
    * @param homePoi
    *   POI to check for
    * @return
    *   boolean, whether home charging is possible
    */
  private def chargingAtHomePossibleForHomePOI(
      homePoi: PointOfInterest,
      chargingStations: Seq[ChargingStation]
  ): Boolean = {
    homePoi.nearestChargingStations.foldLeft(false)(
      (
          chargeAtHome: Boolean,
          cs: (ChargingStation, ComparableQuantity[Length])
      ) => {
        val evcsIsHomeType: Boolean =
          chargingStations
            .find(_ == cs._1)
            .exists(_.evcsLocationType == EvcsLocationType.HOME)
        chargeAtHome || evcsIsHomeType
      }
    )
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
  private def assignInitialHomeChargingCars(
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

  private def assignInitialHomeChargingCars(
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

  private def buildEvWithType(
      id: String,
      evType: EvType,
      workPoiPdf: ProbabilityDensityFunction[PointOfInterest],
      firstDepartureOfDay: FirstDepartureOfDay,
      startTime: ZonedDateTime,
      homePoi: PointOfInterest,
      isHomeChargingPossible: Boolean
  ): ElectricVehicle = {
    /* Sample work POI based on sizes */
    val workPoi = workPoiPdf.sample()
    /* Like this, the EV will have its first departure on first day */
    val firstDeparture = firstDepartureOfDay.sample(
      startTime.minusDays(1)
    )

    buildEv(
      id,
      evType,
      homePoi,
      workPoi,
      startTime,
      firstDeparture,
      isHomeChargingPossible
    )
  }

  /** Build an [[ElectricVehicle]] from given information
    *
    * @param id
    *   Human-readable identifier of the ev
    * @param evType
    *   Type
    * @param homePoi
    *   Home point of interest
    * @param workPoi
    *   Work point of interest
    * @param simulationStart
    *   Simulation start date
    * @param firstDeparture
    *   Wall clock time of first departure
    * @param isChargingAtHomePossible
    *   If home charging is possible or not
    * @return
    *   An equivalent EV model
    */
  def buildEv(
      id: String,
      evType: EvType,
      homePoi: PointOfInterest,
      workPoi: PointOfInterest,
      simulationStart: ZonedDateTime,
      firstDeparture: ZonedDateTime,
      isChargingAtHomePossible: Boolean
  ): ElectricVehicle = {
    ElectricVehicle(
      simulationStart = simulationStart,
      uuid = UUID.randomUUID(),
      id = id,
      // todo: check if this is neccessary
      evType = evType.copy(
        dcPower =
          if (
            evType.dcPower.isLessThan(
              Quantities.getQuantity(0.1, PowerSystemUnits.KILOWATT)
            )
          ) evType.acPower
          else evType.dcPower
      ),
      homePoi = homePoi,
      workPoi = workPoi,
      storedEnergy = evType.capacity,
      destinationPoiType = PoiTypeDictionary.HOME,
      destinationPoi = homePoi, // is updated when trip is sampled
      parkingTimeStart = simulationStart, // EV starts parking at first tick
      departureTime =
        if (firstDeparture == simulationStart)
          firstDeparture.plusMinutes(1)
        else firstDeparture,
      chargingAtHomePossible = isChargingAtHomePossible,
      chosenChargingStation = None,
      chargingAtSimona = false,
      finalDestinationPoi = None,
      finalDestinationPoiType = None,
      remainingDistanceAfterChargingHub = None,
      chargingPricesMemory = Queue[Double]()
    )
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
  private def assignRemainingCars(
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

  private def assignRemainingCars(
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
}
