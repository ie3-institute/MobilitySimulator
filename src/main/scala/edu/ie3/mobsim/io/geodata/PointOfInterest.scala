/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.model.ChargingStation
import edu.ie3.util.geo.GeoUtils
import edu.ie3.util.quantities.PowerSystemUnits
import org.locationtech.jts.geom.Coordinate
import squants.Length
import squants.space.Kilometers

import java.util.UUID
import scala.collection.parallel.CollectionConverters.seqIsParallelizable
import scala.collection.parallel.ParSeq
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.jdk.CollectionConverters._
import scala.util.{Try, Using}

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
    nearestChargingStations: Map[ChargingStation, Length],
) extends Ordered[PointOfInterest] {
  def compare(that: PointOfInterest): Int = {
    if (this.id == that.id) 0
    else if (this.id > that.id) 1
    else -1
  }

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
      evcs: Seq[ChargingStation],
      maxDistanceFromPoi: Length,
      maxDistanceFromHomePoi: Length,
      assignHomeNearestChargingStations: Boolean,
  ): Try[Seq[PointOfInterest]] =
    Using(Source.fromFile(filePath).bufferedReader()) { reader =>
      /* Determine order of headline */
      val header = reader.readLine().trim.toLowerCase.split(csvSep)
      val colToIndex = assessHeadLine(header)
      val catLocationIdx = colToIndex.getOrElse(
        categoricalLocation,
        throw new RuntimeException("$categoricalLocation not found"),
      )

      /* Prepare charging stations */
      val locationTypeToChargingStations = prepareChargingStations(evcs)

      reader
        .lines()
        .iterator()
        .asScala
        .toSeq
        .par
        .map(_.split(csvSep))
        .partition { entries =>
          entries(
            catLocationIdx
          ).toLowerCase == CategoricalLocationDictionary.HOME.toString.toLowerCase
        } match {
        case (homePois, otherPois) =>
          /* Home POIs have to be treated sequentially */
          val homeCs = suitableChargingStations(
            CategoricalLocationDictionary.HOME,
            locationTypeToChargingStations,
          )
          val homeFuture = parseHomePoi(
            homePois,
            colToIndex,
            homeCs,
            maxDistanceFromHomePoi,
            assignHomeNearestChargingStations,
          )
          /* Other POIs can be treated in parallel */
          val otherFuture = Future(
            otherPois
              .map(entries =>
                parse(
                  entries,
                  colToIndex,
                  locationTypeToChargingStations,
                  maxDistanceFromPoi,
                )
              )
              .seq
          )

          Await.result(
            for {
              homePois <- homeFuture
              otherPois <- otherFuture
            } yield homePois ++ otherPois,
            Duration("1hr"),
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
      chargingStations: Seq[ChargingStation]
  ): Map[EvcsLocationType, Seq[ChargingStation]] =
    chargingStations
      .filterNot { evcs =>
        evcs.evcsLocationType == EvcsLocationType.CHARGING_HUB_HIGHWAY ||
        evcs.evcsLocationType == EvcsLocationType.CHARGING_HUB_TOWN
      }
      .groupBy(_.evcsLocationType)

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
      entries: ParSeq[Array[String]],
      colToIndex: Map[String, Int],
      chargingStations: Seq[ChargingStation],
      maxDistance: Length,
      assignNearestChargingStation: Boolean,
  ): Future[Seq[PointOfInterest]] =
    Future {
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
          Map.empty[ChargingStation, Length],
        )
        val nearestCs =
          if (assignNearestChargingStation)
            nearbyChargingStations(
              chargingStations,
              poi.geoPosition,
              maxDistance,
            )
          else
            Seq.empty
        poi -> nearestCs.sortBy(_._2)
      }.toList
    }
      .map(assignHomeChargingStations)

  /** Per home POI, assign the nearest home charging station. Each charging
    * station is assigned to only one single home POI.
    *
    * @param poiWithNearbyChargingStations
    *   POIs with their nearest charging stations
    * @return
    *   A [[Seq]] of [[PointOfInterest]]s with its nearest home charging station
    *   assigned, if available
    */
  private def assignHomeChargingStations(
      poiWithNearbyChargingStations: Seq[
        (
            PointOfInterest,
            Seq[(ChargingStation, Length)],
        )
      ]
  ): Seq[PointOfInterest] =
    poiWithNearbyChargingStations
      .sortBy { case (poi, _) =>
        poi.id
      }
      .foldLeft((Seq.empty[ChargingStation], Seq.empty[PointOfInterest])) {
        case (
              (alreadyAssignedChargingStations, adaptedPois),
              (poi, nearestChargingStations),
            ) =>
          nearestChargingStations.find { case (cs, _) =>
            !alreadyAssignedChargingStations.contains(cs)
          } match {
            case Some((cs, distance)) =>
              val updatedPoi =
                poi.copy(nearestChargingStations = Map(cs -> distance))
              (
                alreadyAssignedChargingStations :+ cs,
                adaptedPois :+ updatedPoi,
              )
            case None =>
              /* No home cs can be assigned to this poi, hand it back un-altered */
              (alreadyAssignedChargingStations, adaptedPois :+ poi)
          }
      } match {
      case (_, pois) => pois
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
      locationToChargingStations: Map[EvcsLocationType, Seq[ChargingStation]],
      maxDistance: Length,
  ): PointOfInterest = {
    val (uuid, identifier, sze, coordinate, catLoc) = parse(entries, colToIndex)

    val allowedChargingStations =
      suitableChargingStations(
        catLoc,
        locationToChargingStations,
      )

    val nearestChargingStations =
      nearbyChargingStations(
        allowedChargingStations,
        coordinate,
        maxDistance,
      ).toMap

    new PointOfInterest(
      uuid,
      identifier,
      catLoc,
      coordinate,
      sze,
      nearestChargingStations,
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
      PoiEnums.CategoricalLocationDictionary.Value,
  ) = {
    val entityFieldMap = colToIndex.map { case (head, idx) =>
      head -> entries(idx)
    }
    val uuidValue = UUID.fromString(
      entityFieldMap.getOrElse(
        uuid,
        throw InitializationException(
          "Unable to get the unique identifier of a point of interest."
        ),
      )
    )
    val identifier = entityFieldMap.getOrElse(
      id,
      throw InitializationException(
        "Unable to get the human readable id of a point of interest."
      ),
    )
    val sze = entityFieldMap
      .getOrElse(
        size,
        throw InitializationException(
          "Unable to get the size of a point of interest."
        ),
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
      locationToChargingStations: Map[EvcsLocationType, Seq[ChargingStation]],
  ): Seq[ChargingStation] =
    categoricalLocation match {
      case CategoricalLocationDictionary.HOME =>
        locationToChargingStations
          .getOrElse(EvcsLocationType.HOME, Seq.empty[ChargingStation])
      case CategoricalLocationDictionary.WORK =>
        locationToChargingStations
          .getOrElse(EvcsLocationType.WORK, Seq.empty[ChargingStation])
      case _ =>
        locationToChargingStations
          .filterNot { case (locationType, _) =>
            locationType == EvcsLocationType.HOME || locationType == EvcsLocationType.WORK
          }
          .values
          .flatten
          .toSeq
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
      allowedChargingStations: Seq[ChargingStation],
      poiCoordinate: Coordinate,
      maxDistance: Length,
  ): Seq[(ChargingStation, Length)] =
    allowedChargingStations
      .map { evcs =>
        evcs -> Kilometers(
          GeoUtils
            .calcHaversine(
              poiCoordinate.y,
              poiCoordinate.x,
              evcs.geoPosition.y,
              evcs.geoPosition.x,
            )
            .to(PowerSystemUnits.KILOMETRE)
            .getValue
            .doubleValue()
        )
      }
      .filter(_._2 < maxDistance)

}
