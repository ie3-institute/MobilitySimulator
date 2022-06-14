/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.io.geodata.PoiEnums
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import tech.units.indriya.ComparableQuantity
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import tech.units.indriya.quantity.Quantities.getQuantity

import java.time.temporal.ChronoUnit
import java.util.UUID
import javax.measure.quantity.{Energy, Length, Power}
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.math.Ordering.Implicits.infixOrderingOps
import scala.util.Random

object ChargingBehavior extends LazyLogging {

  /* Probabilities for starting a charging session for EvcsLocationTypes, baed on [Windt.2020] */
  val PROB_WITH_HOME_CHARGING: Map[EvcsLocationType, Double] = Map(
    EvcsLocationType.HOME -> 1d,
    EvcsLocationType.WORK -> 0.48d,
    EvcsLocationType.CUSTOMER_PARKING -> 0.34d,
    EvcsLocationType.STREET -> 0.06d
  )
  val PROB_WITHOUT_HOME_CHARGING: Map[EvcsLocationType, Double] = Map(
    EvcsLocationType.HOME -> 0d,
    EvcsLocationType.WORK -> 1d,
    EvcsLocationType.CUSTOMER_PARKING -> 0.61d,
    EvcsLocationType.STREET -> 0.3d
  )

  /** Chose a charging station to charge at for the EV based on available
    * information. The EV can either chose a charging station, or return "None"
    * if it doesn't want to charge at all.
    *
    * @param ev
    *   The EV with trip information including its destination and the nearest
    *   charging stations
    * @param currentPricesAtChargingStations
    *   current prices at charging stations
    * @param currentlyAvailableChargingPoints
    *   currently available charging points at all charging stations
    * @param seed
    *   Random seed
    * @param maxDistance
    *   Maximum permissible distance between a POI and a charging station
    * @return
    *   UUID of the charging station the EV wants to charge at, or None if it
    *   doesn't want to charge anywhere
    */
  def chooseChargingStation(
      ev: ElectricVehicle,
      currentPricesAtChargingStations: Map[UUID, java.lang.Double],
      currentlyAvailableChargingPoints: Map[UUID, Integer],
      seed: Random,
      maxDistance: ComparableQuantity[Length]
  ): Option[UUID] = {
    /* If there are charging stations nearby */
    if (ev.getDestinationPoi.nearestChargingStations.nonEmpty) {
      /* Always charge if the EV makes charging stop at charging hub */
      if (
        ev.getDestinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_TOWN
        || ev.getDestinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_HIGHWAY
      ) {
        logger.debug(
          s"${ev.getId} arrives at charging hub and wants to start charging."
        )
        ev.getDestinationPoi.nearestChargingStations.keys.headOption
          .map(_.getUuid)
      } else {
        /* Update charging prices memory of EV to have a reference for the prices of specific charging stations */
        ev.getDestinationPoi.nearestChargingStations.keys.foreach { cs =>
          currentPricesAtChargingStations
            .get(cs.getUuid)
            .map(ev.updateChargingPricesMemory(_))
        }

        /* If EV wants to charge, rank available charging stations and choose the best */
        val evWantsToCharge = doesEvWantToCharge(ev, seed)
        if (evWantsToCharge) {
          /* Create rating for each possible charging station */
          createRatingsForChargingStations(
            ev,
            currentPricesAtChargingStations,
            currentlyAvailableChargingPoints,
            maxDistance
          ).maxByOption(_._2).map(_._1)
        } else {
          logger.debug(
            s"${ev.getId} could charge but decides against charging."
          )
          None
        }
      }
      /* If there are no charging stations nearby */
    } else {
      logger.debug(
        s"${ev.getId} can't charge because there are no charging stations nearby."
      )
      None
    }
  }

  /** Determine whether an EV wants to charge at its destination based on its
    * SoC and parking time.
    * @param ev
    *   The EV for which the decision shall be made
    * @param seed
    *   random seed
    * @return
    *   Decision as boolean, whether the EV wants to charge
    */
  def doesEvWantToCharge(ev: ElectricVehicle, seed: Random): Boolean = {
    val staysLongEnough = ev.getParkingTimeStart
      .until(ev.getDepartureTime, ChronoUnit.MINUTES) >= 15
    if (staysLongEnough) {
      val soc = ev.getStoredEnergy.divide(ev.getEStorage).getValue.doubleValue()

      /* Probability that the EV wants to charge based on its SoC.
      Distinguish between home and work and elsewhere for threshold for charging */
      val (lowerThreshold, upperThreshold) = {
        if (ev.isChargingAtHomePossible) {
          if (
            ev.getDestinationCategoricalLocation == CategoricalLocationDictionary.HOME
            || ev.getDestinationCategoricalLocation == CategoricalLocationDictionary.WORK
          ) {
            (0.4, 0.85)
          } else {
            (0.3, 0.5)
          }
        } else {
          if (
            ev.getDestinationCategoricalLocation == CategoricalLocationDictionary.HOME
            || ev.getDestinationCategoricalLocation == CategoricalLocationDictionary.WORK
          ) {
            (0.4, 0.85)
          } else {
            (0.3, 0.75)
          }
        }
      }

      if (soc < lowerThreshold) {
        true
      } else if (soc > upperThreshold) {
        false
      } else {
        val probabilityFromSoC =
          1 - ((soc - lowerThreshold) / (upperThreshold - lowerThreshold))
        /* Sample if EV wants to charge based on SoC */
        seed.nextDouble() < probabilityFromSoC
      }
    } else false
  }

  /** Create a rating for each charging station based on different parameters.
    * Each parameter gets a rating between 0 and 1, where 0 is worst and 1 is
    * best. Finally, the ratings are combined with a possible weighting.
    *
    * @param ev
    *   The EV for which the available charging stations shall be rated
    * @param currentPricesAtChargingStations
    *   Map of current prices at all charging stations
    * @param currentlyAvailableChargingPoints
    *   Map of available charging points at all charging stations
    * @param maxDistance
    *   Maximum permissible distance between a POI and a charging station
    * @return
    *   Map with a charging stations and their rating
    */
  def createRatingsForChargingStations(
      ev: ElectricVehicle,
      currentPricesAtChargingStations: Map[UUID, java.lang.Double],
      currentlyAvailableChargingPoints: Map[UUID, Integer],
      maxDistance: ComparableQuantity[Length]
  ): Map[UUID, Double] =
    ev.getDestinationPoi.nearestChargingStations.par
      .map { case (cs, distance) =>
        /* Check if free charging spots are even available */
        val freeSpots: Integer =
          currentlyAvailableChargingPoints.getOrElse(cs.getUuid, 0)

        /* If no spots are available, the rating is 0. Otherwise, calculate a rating */
        val rating = if (freeSpots < 1) {
          0
        } else {
          /* Rating for distance between 0 and 1 */
          val ratingForDistance = distanceRating(distance, maxDistance)

          /* Rating for possible charging amount between 0 and 1 */
          val ratingForChargeableEnergy = chargeableEnergyRating(cs, ev)

          /* Rating for price between 0 and 1; if price not available use 1 (assumption: charging is for free) */
          val ratingForPrice =
            priceRating(cs, ev, currentPricesAtChargingStations)

          /* Return charging station UUID with sum of weighted ratings */
          // TODO: Improve weighting of ratings based on literature or testing?
          ratingForDistance + ratingForChargeableEnergy * 2 + ratingForPrice * 2
        }
        cs.getUuid -> rating
      }
      .seq
      .toMap

  private def distanceRating(
      distance: ComparableQuantity[Length],
      maxDistance: ComparableQuantity[Length]
  ): Double =
    1 - distance
      .divide(maxDistance)
      .getValue
      .doubleValue()

  private def chargeableEnergyRating(
      cs: ChargingStation,
      ev: ElectricVehicle
  ): Double = {
    val availableChargingPowerForEV =
      cs.getEvcsType.getElectricCurrentType match {
        case ElectricCurrentType.AC =>
          ev.getSRatedAC.min(cs.getEvcsType.getsRated()).to(KILOWATT)
        case ElectricCurrentType.DC =>
          ev.getSRatedDC.min(cs.getEvcsType.getsRated()).to(KILOWATT)
      }
    val parkingTime =
      ev.getParkingTimeStart
        .until(ev.getDepartureTime, ChronoUnit.MINUTES)
    val possibleChargeableEnergy =
      getQuantity(
        availableChargingPowerForEV
          .multiply(parkingTime * 60)
          .getValue
          .doubleValue(),
        KILOWATTHOUR
      )
    val requiredEnergy =
      ev.getEStorage.subtract(ev.getStoredEnergy).to(KILOWATTHOUR)
    math.min(
      possibleChargeableEnergy.divide(requiredEnergy).getValue.doubleValue(),
      1.0
    )
  }

  private def priceRating(
      cs: ChargingStation,
      ev: ElectricVehicle,
      currentPrices: Map[UUID, java.lang.Double]
  ): Double = currentPrices.get(cs.getUuid) match {
    case Some(price) =>
      ev.getChargingPricesMemory.maxOption.zip(
        ev.getChargingPricesMemory.minOption
      ) match {
        case Some((maxPrice, minPrice)) =>
          if (math.abs(maxPrice - minPrice) < 0.001)
            0.5 // if all prices are the same; avoid dividing by 0
          else
            (maxPrice - price.toDouble) / (maxPrice - minPrice)
        case None => 1
      }
    case None => 1
  }
}
