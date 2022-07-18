/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.{
  CategoricalLocationDictionary,
  PoiTypeDictionary
}
import edu.ie3.mobsim.model.ChargingStation
import edu.ie3.util.geo.GeoUtils
import org.locationtech.jts.geom.Coordinate
import tech.units.indriya.ComparableQuantity

import java.util.UUID
import javax.measure.Quantity
import javax.measure.quantity.Length
import scala.collection.immutable.TreeSet
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Try, Using}
import scala.jdk.CollectionConverters._

/** A point of special interest (POI)
  *
  * @param uuid
  *   Unique identifier of the point of interest
  * @param id
  *   Human-readable identifier
  * @param categoricalLocation
  *   Type of categorical location, the POI describes
  * @param geoPosition
  *   Geographic position
  * @param size
  *   Sizing of the POI (e.g. the size of the shop)
  * @param nearestChargingStations
  *   Mapping of nearest charging stations from their UUID to distance
  */
final case class PointOfInterest(
    uuid: UUID,
    id: String,
    categoricalLocation: CategoricalLocationDictionary.Value,
    geoPosition: Coordinate,
    size: Double,
    nearestChargingStations: Map[ChargingStation, ComparableQuantity[Length]]
) extends Ordered[PointOfInterest] {
  def compare(that: PointOfInterest): Int = {
    if (this.id == that.id) 0
    else if (this.id > that.id) 1
    else -1
  }

  def getPoiType: PoiTypeDictionary.Value =
    PoiTypeDictionary.apply(categoricalLocation)
}

case object PointOfInterest {
  /* Columns in a csv file to describe the content */
  private val uuid = "uuid"
  private val id = "id"
  private val size = "size"
  private val lat = "lat"
  private val lon = "lon"
  private val categoricalLocation = "categoricallocation"
  private val columns = Seq(uuid, id, size, lat, lon, categoricalLocation)

  def getFromFile(
      filePath: String,
      csvSep: String,
      evcs: Set[ChargingStation],
      maxDistanceFromPoi: ComparableQuantity[Length],
      maxDistanceFromHomePoi: ComparableQuantity[Length]
  ): Try[Set[PointOfInterest]] =
    Using(Source.fromFile(filePath).bufferedReader()) { reader =>
      /* Determine order of headline */
      val header = reader.readLine().trim.toLowerCase.split(csvSep)
      val colToIndex = assessHeadLine(header)

      /* Prepare charging stations */
      val locationTypeToChargingStations = prepareChargingStations(evcs)

      reader.lines().iterator().asScala.map(_.split(csvSep)).toSeq.partition {
        entries =>
          colToIndex
            .get(categoricalLocation)
            .map(idx => entries(idx))
            .exists(
              _.toLowerCase == CategoricalLocationDictionary.HOME.toString.toLowerCase
            )
      } match {
        case (homePois, otherPois) =>
          /* Home POIs have to be treated sequentially */
          val homeCs = locationTypeToChargingStations.getOrElse(
            EvcsLocationType.HOME,
            throw InitializationException(
              "Unable to get home charging stations."
            )
          )
          val homeFuture = parseHomePoi(
            homePois,
            colToIndex,
            homeCs,
            maxDistanceFromHomePoi
          )
          /* Other POIs can be treated parallely */
          val otherFuture = Future.sequence(
            otherPois
              .map(entries =>
                Future(
                  parse(
                    entries,
                    colToIndex,
                    locationTypeToChargingStations,
                    maxDistanceFromPoi
                  )
                )
              )
              .toSet
          )

          Await.result(
            for {
              homePois <- homeFuture
              otherPois <- otherFuture
            } yield homePois ++ otherPois,
            Duration("1hr")
          )
      }
    }

  /** Check the headline and determine the position of the needed elements
    * @param header
    *   Elements in the given header
    * @return
    *   A mapping from column to column index
    */
  private def assessHeadLine(header: Array[String]): Map[String, Int] = {
    if (header.length != columns.length)
      throw InitializationException(
        s"Unable to read POI from file. Expected headline elements: '${columns
          .mkString(",")}', given elements: '${header.mkString(",")}'"
      )
    columns.map { element =>
      val idx = header.indexOf(element)
      if (idx < 0)
        throw InitializationException(
          s"Unable to obtain position of column '$element'"
        )
      element -> idx
    }.toMap
  }

  /** Filter charging stations, that can be assigned to points of interest and
    * group them by their location type. Charging hubs are not suitable for
    * this.
    *
    * @param chargingStations
    *   Collection of available charging stations
    * @return
    *   A mapping from [[EvcsLocationType]] to charging stations
    */
  private def prepareChargingStations(
      chargingStations: Set[ChargingStation]
  ): Map[EvcsLocationType, Set[ChargingStation]] =
    chargingStations
      .filterNot { evcs =>
        evcs.getEvcsLocationType == EvcsLocationType.CHARGING_HUB_HIGHWAY ||
        evcs.getEvcsLocationType == EvcsLocationType.CHARGING_HUB_TOWN ||
        evcs.isHomeChargingStationAssignedToPOI
      }
      .groupBy(_.getEvcsLocationType)

  /** Parse Points of Interest of type home. First, all distances between POI
    * coordinates and charging stations are calculated asynchronously, filtered
    * for the maximum permissible distance and ordered by distance. Afterwards,
    * the nearest charging station is assigned to each point of interest, taking
    * into account, that a charging station can only be assigned to one home
    * POI. Assignment is carried out synchronously to maintain the same results
    * for multiple simulation runs.
    *
    * @param entries
    *   Iterator for string values of each POI
    * @param colToIndex
    *   Mapping from column header to index of the values
    * @param chargingStations
    *   Collection of available home charging points
    * @param maxDistance
    *   Maximum permissible distance between home POI and charging station
    * @return
    *   Home POI with assigned home charging station, if available
    */
  private def parseHomePoi(
      entries: Seq[Array[String]],
      colToIndex: Map[String, Int],
      chargingStations: Set[ChargingStation],
      maxDistance: ComparableQuantity[Length]
  ): Future[Set[PointOfInterest]] =
    Future
      .sequence {
        /* Build mapping from POI to distance asynchronously */
        entries.map { values =>
          /* Parse the attributes of the POI */
          val (uuid, identifier, sze, coordinate, catLoc) =
            parse(values, colToIndex)
          val poi = PointOfInterest(
            uuid,
            identifier,
            catLoc,
            coordinate,
            sze,
            Map.empty[ChargingStation, ComparableQuantity[Length]]
          )
          findNearestChargingStations(
            poi,
            chargingStations,
            maxDistance
          )
        }
      }
      .map(assignChargingStations)

  private type PoiToNearbyChargingStations =
    (PointOfInterest, TreeSet[(ChargingStation, ComparableQuantity[Length])])

  /** Find the nearest charging stations in an asynchronous fashion, determine
    * the distance and filter for a maximum permissible distance
    *
    * @param poi
    *   Point of interest
    * @param chargingStations
    *   Available charging stations
    * @param maxDistance
    *   Maximum permissible distance
    * @return
    *   A [[Future]] onto the [[PointOfInterest]] with its closest
    *   [[ChargingStation]]s
    */
  private def findNearestChargingStations(
      poi: PointOfInterest,
      chargingStations: Set[ChargingStation],
      maxDistance: ComparableQuantity[Length]
  ): Future[PoiToNearbyChargingStations] =
    Future
      .sequence(chargingStations.map(distance(_, poi.geoPosition)))
      .map {
        /* Filter out the charging stations, that are too far away */
        _.filter(_._2.isLessThan(maxDistance))
      }
      .map {
        /* Order the entries by distance ascending */
        TreeSet.from(_)(Ordering.by(_._2))
      }
      .map(poi -> _)

  /** Determine the distance between a charging station and a given coordinate
    *
    * @param chargingStation
    *   The charging station
    * @param coordinate
    *   The coordination in question
    * @return
    *   Future onto the distance between both
    */
  private def distance(
      chargingStation: ChargingStation,
      coordinate: Coordinate
  ): Future[(ChargingStation, ComparableQuantity[Length])] = Future {
    chargingStation -> GeoUtils.calcHaversine(
      coordinate.y,
      coordinate.x,
      chargingStation.getGeoPosition.y,
      chargingStation.getGeoPosition.x
    )
  }

  /** Per Point of Interest, assign the nearest home charging station, taking
    * care of only assigning a charging station to one home POI
    *
    * @param poiWithNearbyChargingStations
    *   Iterator of POI with its nearest charging stations
    * @return
    *   A [[Set]] of [[PointOfInterest]]s with its nearest home charging station
    *   assigned
    */
  private def assignChargingStations(
      poiWithNearbyChargingStations: Seq[PoiToNearbyChargingStations]
  ): Set[PointOfInterest] =
    poiWithNearbyChargingStations
      .sortBy { case (poi, _) =>
        poi.id
      }
      .foldLeft((Seq.empty[ChargingStation], Seq.empty[PointOfInterest])) {
        case (
              (alreadyAssignedChargingStations, adaptedPois),
              (poi, nearestChargingStations)
            ) =>
          nearestChargingStations.find { case (cs, _) =>
            !alreadyAssignedChargingStations.contains(cs)
          } match {
            case Some((cs, distance)) =>
              val updatedPoi =
                poi.copy(nearestChargingStations = Map(cs -> distance))
              (
                alreadyAssignedChargingStations :+ cs,
                adaptedPois :+ updatedPoi
              )
            case None =>
              /* No home cs can be assigned to this poi, hand it back un-altered */
              (alreadyAssignedChargingStations, adaptedPois :+ poi)
          }
      } match {
      case (_, pois) => pois.toSet
    }

  /** Parse a given amount of String entries to a [[PointOfInterest]]
    *
    * @param entries
    *   String entries
    * @param colToIndex
    *   Mapping from header element to index of entry
    * @param locationToChargingStations
    *   Mapping from location type to charging stations
    * @param maxDistance
    *   Maximum permissible distance between a POI and a charging station
    * @return
    *   A [[PointOfInterest]]
    */
  private def parse(
      entries: Array[String],
      colToIndex: Map[String, Int],
      locationToChargingStations: Map[EvcsLocationType, Set[ChargingStation]],
      maxDistance: ComparableQuantity[Length]
  ): PointOfInterest = {
    val (uuid, identifier, sze, coordinate, catLoc) = parse(entries, colToIndex)

    val nearestChargingStations =
      findNearestChargingStations(
        catLoc,
        coordinate,
        locationToChargingStations,
        maxDistance
      )

    new PointOfInterest(
      uuid,
      identifier,
      catLoc,
      coordinate,
      sze,
      nearestChargingStations
    )
  }

  /** Parse the content of the line to actual properties
    *
    * @param entries
    *   Collection of [[String]] entries
    * @param colToIndex
    *   Mapping from column header to index
    * @return
    *   The properties
    */
  private def parse(entries: Array[String], colToIndex: Map[String, Int]): (
      UUID,
      String,
      Double,
      Coordinate,
      PoiEnums.CategoricalLocationDictionary.Value
  ) = {
    val entityFieldMap = colToIndex.map { case (head, idx) =>
      head -> entries(idx)
    }
    val uuidValue = UUID.fromString(
      entityFieldMap.getOrElse(
        uuid,
        throw InitializationException(
          "Unable to get the unique identifier of a point of interest."
        )
      )
    )
    val identifier = entityFieldMap.getOrElse(
      id,
      throw InitializationException(
        "Unable to get the human readable id of a point of interest."
      )
    )
    val sze = entityFieldMap
      .getOrElse(
        size,
        throw InitializationException(
          "Unable to get the size of a point of interest."
        )
      )
      .toDouble
    val coordinate = entityFieldMap
      .get(lat)
      .zip(entityFieldMap.get(lon))
      .map { case (lat, lon) =>
        new Coordinate(lon.toDouble, lat.toDouble)
      }
      .getOrElse(
        throw InitializationException(
          "Unable to get the position of a point of interest."
        )
      )
    val catLoc = entityFieldMap
      .get(categoricalLocation)
      .map(CategoricalLocationDictionary(_))
      .getOrElse(
        throw InitializationException(
          "Unable to get categorical location of a point of interest."
        )
      )

    (uuidValue, identifier, sze, coordinate, catLoc)
  }

  /** Find nearest charging station for a POI.
    *
    * @param poiCoordinate
    *   coordinates of the POI
    * @param locationToChargingStations
    *   Mapping from location type to charging stations
    * @return
    *   UUID of nearest charging station
    */
  private def findNearestChargingStations(
      categoricalLocation: CategoricalLocationDictionary.Value,
      poiCoordinate: Coordinate,
      locationToChargingStations: Map[EvcsLocationType, Set[ChargingStation]],
      maxDistance: Quantity[Length]
  ): Map[ChargingStation, ComparableQuantity[Length]] = {
    val allowedChargingStations =
      suitableChargingStations(
        categoricalLocation,
        locationToChargingStations
      )

    nearbyChargingStations(
      allowedChargingStations,
      poiCoordinate,
      maxDistance
    )
  }

  /** Get the suitable charging stations for the given categorical location.
    *
    * @param categoricalLocation
    *   categorical location / POI type of the POI to find the charging stations
    *   for
    * @param locationToChargingStations
    *   Mapping from location type to charging stations
    * @return
    *   all allowed charging stations for this POI type
    */
  private def suitableChargingStations(
      categoricalLocation: CategoricalLocationDictionary.Value,
      locationToChargingStations: Map[EvcsLocationType, Set[ChargingStation]]
  ): Set[ChargingStation] =
    categoricalLocation match {
      case CategoricalLocationDictionary.HOME =>
        locationToChargingStations
          .getOrElse(EvcsLocationType.HOME, List.empty[ChargingStation])
          .filterNot(_.isHomeChargingStationAssignedToPOI)
          .toSet
      case CategoricalLocationDictionary.WORK =>
        locationToChargingStations
          .getOrElse(EvcsLocationType.WORK, List.empty[ChargingStation])
          .toSet
      case _ =>
        locationToChargingStations
          .filterNot { case (locationType, _) =>
            locationType == EvcsLocationType.HOME || locationType == EvcsLocationType.WORK
          }
          .values
          .flatten
          .toSet
    }

  /** Among the allowed charging stations, find all that are within range of the
    * POI (defined by maximumDistance). Using calcHaversine method to calculate
    * distances between POI and charging stations.
    *
    * @param allowedChargingStations
    *   already filtered allowed charging stations for the POI
    * @param poiCoordinate
    *   coordinates for the POI
    * @return
    *   all allowed charging stations within range of the POI
    */
  private def nearbyChargingStations(
      allowedChargingStations: Set[ChargingStation],
      poiCoordinate: Coordinate,
      maxDistance: Quantity[Length]
  ): Map[ChargingStation, ComparableQuantity[Length]] =
    allowedChargingStations
      .map { evcs =>
        evcs -> GeoUtils.calcHaversine(
          poiCoordinate.y,
          poiCoordinate.x,
          evcs.getGeoPosition.y,
          evcs.getGeoPosition.x
        )
      }
      .filter(_._2.isLessThan(maxDistance))
      .toMap
}
