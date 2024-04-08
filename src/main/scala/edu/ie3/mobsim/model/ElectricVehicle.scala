/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.io.geodata.PointOfInterest
import edu.ie3.mobsim.io.probabilities.{FirstDepartureOfDay, ProbabilityDensityFunction}
import edu.ie3.mobsim.utils.utils.toTick
import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.Kilowatts
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

  def getEStorage: ComparableQuantity[Energy] =
    Quantities.getQuantity(
      evType.capacity.toKilowattHours,
      PowerSystemUnits.KILOWATTHOUR
    )

  def getSRatedAC: ComparableQuantity[Power] = Quantities
    .getQuantity(evType.acPower.toKilowatts, PowerSystemUnits.KILOWATT)

  def getSRatedDC: ComparableQuantity[Power] = Quantities
    .getQuantity(evType.dcPower.toKilowatts, PowerSystemUnits.KILOWATT)

  def getStoredEnergy: ComparableQuantity[Energy] =
    storedEnergy

  def getDepartureTick: java.lang.Long = toTick(simulationStart, departureTime)

  /** @param storedEnergy
    *   the new stored energy
    * @return
    *   a copy of this ElectricVehicle with given new stored energy / soc
    */
  def copyWith(
      storedEnergy: ComparableQuantity[Energy]
  ): ElectricVehicle =
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
      remainingDistance: Option[
        ComparableQuantity[Length]
      ]
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

  /** Determine probability density functions for home POI separated by the
    * possible to charge at home or not
    *
    * @param homePoisWithSizes
    *   Mapping from available home POI to it's "size"
    * @param chargingStations
    *   Collection of available charging stations
    * @return
    */
  def determineHomePoiPdf(
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
          cs: (ChargingStation, squants.Length)
      ) => {
        val evcsIsHomeType: Boolean =
          chargingStations
            .find(_ == cs._1)
            .exists(_.evcsLocationType == EvcsLocationType.HOME)
        chargeAtHome || evcsIsHomeType
      }
    )
  }

  def buildEvWithType(
      id: String,
      uuid: UUID,
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
      isHomeChargingPossible,
      uuid
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
      isChargingAtHomePossible: Boolean,
      uuid: UUID = UUID.randomUUID()
  ): ElectricVehicle = {
    ElectricVehicle(
      simulationStart = simulationStart,
      uuid = uuid,
      id = id,
      // todo: check if this is neccessary
      evType = evType.copy(
        dcPower =
          if (evType.dcPower < Kilowatts(0.1)) evType.acPower
          else evType.dcPower
      ),
      homePoi = homePoi,
      workPoi = workPoi,
      storedEnergy = Quantities.getQuantity(
        evType.capacity.toKilowattHours,
        PowerSystemUnits.KILOWATTHOUR
      ),
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

}
