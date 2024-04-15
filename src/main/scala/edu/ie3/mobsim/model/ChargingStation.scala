/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.datamodel.io.source.{RawGridSource, SystemParticipantSource}
import edu.ie3.datamodel.models.ElectricCurrentType
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.io.geodata.PoiEnums
import edu.ie3.util.quantities.PowerSystemUnits.{KILOWATT, KILOWATTHOUR}
import edu.ie3.util.quantities.QuantityUtils.RichQuantity
import org.locationtech.jts.geom.Coordinate
import squants.{Energy, Length, Time}
import squants.energy.Kilowatts
import squants.time.Minutes

import java.time.temporal.ChronoUnit
import java.util.UUID
import scala.collection.immutable.Queue
import scala.collection.parallel.CollectionConverters.ImmutableIterableIsParallelizable
import scala.jdk.CollectionConverters._
import scala.util.Random

final case class ChargingStation(
    uuid: UUID,
    id: String,
    geoPosition: Coordinate,
    evcsType: ChargingPointType,
    evcsLocationType: EvcsLocationType,
    chargingPoints: Int
)

object ChargingStation extends LazyLogging {

  /** Load node and evcs input data using PSDM functions and use it to construct
    * charging stations with needed information. The information contains among
    * others the uuid (same as in SIMONA) and the geographical location
    * (coordinates).
    * @param gridSource
    *   Source to obtain grid information from
    * @param participantSource
    *   Source to obtain participant information from
    * @return
    *   A collection of available [[ChargingStation]]s
    */
  def loadChargingStationsWithPSDM(
      gridSource: RawGridSource,
      participantSource: SystemParticipantSource
  ): Seq[ChargingStation] = {

    val chargingStations = participantSource.getEvcs.asScala.toSeq
      .map { evcs =>
        ChargingStation(
          evcs.getUuid,
          evcs.getId,
          evcs.getNode.getGeoPosition.getCoordinate,
          evcsType = evcs.getType,
          evcs.getLocationType,
          chargingPoints = evcs.getChargingPoints
        )
      }

    logger.info(
      s"Received ${chargingStations.size} charging stations during setup."
    )
    chargingStations
  }

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
    *   doesn't want to charge anywhere and the ev if the ev was updated
    */
  def chooseChargingStation(
      ev: ElectricVehicle,
      currentPricesAtChargingStations: Map[UUID, Double],
      currentlyAvailableChargingPoints: Map[UUID, Int],
      seed: Random,
      maxDistance: Length
  ): (Option[UUID], Option[ElectricVehicle]) = {
    /* If there are charging stations nearby */
    if (ev.destinationPoi.nearestChargingStations.nonEmpty) {
      /* Always charge if the EV makes charging stop at charging hub */
      if (
        ev.destinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_TOWN
        || ev.destinationPoiType == PoiEnums.PoiTypeDictionary.CHARGING_HUB_HIGHWAY
      ) {
        logger.debug(
          s"${ev.getId} arrives at charging hub and wants to start charging."
        )
        (
          ev.destinationPoi.nearestChargingStations.keys.headOption
            .map(_.uuid),
          None
        )
      } else {
        /* Update charging prices memory of EV to have a reference for the prices of specific charging stations */

        val prices = ev.destinationPoi.nearestChargingStations.keys
          .map { cs =>
            currentPricesAtChargingStations(cs.uuid)
          }
          .to(Queue)

        val evWithUpdatedPriceMemory = ev.updateChargingPricesMemory(prices)

        /* Create rating for each possible charging station */
        val ratingMap = createRatingsForChargingStations(
          evWithUpdatedPriceMemory,
          currentPricesAtChargingStations,
          currentlyAvailableChargingPoints,
          maxDistance
        )
        (
          ratingMap.maxByOption { case (_, rating) => rating }.map {
            case (cs, _) => cs
          },
          Some(evWithUpdatedPriceMemory)
        )
      }
      /* If there are no charging stations nearby */
    } else {
      logger.debug(
        s"${ev.getId} can't charge because POI ${ev.destinationPoi.id} does not have charging stations nearby."
      )
      (None, None)
    }
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
      currentPricesAtChargingStations: Map[UUID, Double],
      currentlyAvailableChargingPoints: Map[UUID, Int],
      maxDistance: Length
  ): Map[UUID, Double] =
    ev.destinationPoi.nearestChargingStations.par
      .map { case (cs, distance) =>
        /* Check if free charging spots are even available */
        val freeSpots =
          currentlyAvailableChargingPoints.getOrElse(cs.uuid, 0)

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
        cs.uuid -> rating
      }
      .seq
      .toMap

  private def distanceRating(
      distance: Length,
      maxDistance: Length
  ): Double =
    1 - distance.divide(maxDistance)

  private def chargeableEnergyRating(
      cs: ChargingStation,
      ev: ElectricVehicle
  ): Double = {
    val availableChargingPowerForEV =
      cs.evcsType.getElectricCurrentType match {
        case ElectricCurrentType.AC =>
          Kilowatts(
            ev.getSRatedAC
              .min(
                cs.evcsType
                  .getsRated()
              )
              .to(KILOWATT)
              .getValue
              .doubleValue()
          )

        case ElectricCurrentType.DC =>
          Kilowatts(
            ev.getSRatedDC
              .min(cs.evcsType.getsRated())
              .to(KILOWATT)
              .getValue
              .doubleValue()
          )
      }
    val parkingTime: Time = Minutes(
      ev.parkingTimeStart
        .until(ev.departureTime, ChronoUnit.MINUTES)
    )
    val possibleChargeableEnergy: Energy =
      availableChargingPowerForEV * parkingTime
    val requiredEnergy = ev.getEStorage.subtract(ev.getStoredEnergy)
    if (requiredEnergy.to(KILOWATTHOUR).getValue.doubleValue() == 0) 1
    else {
      math.min(
        possibleChargeableEnergy.toKilowattHours / requiredEnergy
          .to(KILOWATTHOUR)
          .getValue
          .doubleValue(),
        1.0
      )
    }
  }

  private def priceRating(
      cs: ChargingStation,
      ev: ElectricVehicle,
      currentPrices: Map[UUID, Double]
  ): Double = currentPrices.get(cs.uuid) match {
    case Some(price) =>
      ev.chargingPricesMemory.maxOption.zip(
        ev.chargingPricesMemory.minOption
      ) match {
        case Some((maxPrice, minPrice)) =>
          if (math.abs(maxPrice - minPrice) < 0.001)
            0.5 // if all prices are the same; avoid dividing by 0
          else
            (maxPrice - price) / (maxPrice - minPrice)
        case None => 1
      }
    case None => 1
  }
}
