/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import com.typesafe.scalalogging.LazyLogging
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.probabilities.ProbabilityDensityFunction
import edu.ie3.mobsim.model.ChargingStation
import tech.units.indriya.ComparableQuantity

import javax.measure.quantity.Length
import scala.collection.immutable.{SortedMap, TreeSet}
import scala.util.{Failure, Success}

object PoiUtils extends LazyLogging {

  /** Load the points of interests from file
    *
    * @param chargingStations
    *   Available charging stations
    * @param poiPath
    *   Path to the directories, where all POIs are located
    * @return
    *   A mapping from categorical location type to Points of Interest
    */
  def loadPOIs(
      chargingStations: Seq[ChargingStation],
      poiPath: String,
      maxDistanceFromPoi: ComparableQuantity[Length],
      maxDistanceFromHomePoi: ComparableQuantity[Length]
  ): Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]] = {
    val start = System.currentTimeMillis()
    PointOfInterest
      .getFromFile(
        poiPath,
        ",",
        chargingStations,
        maxDistanceFromPoi,
        maxDistanceFromHomePoi
      )
      .map { allPois =>
        allPois.groupBy(_.categoricalLocation).map { case (catLoc, pois) =>
          catLoc -> TreeSet.from(pois)
        }
      } match {
      case Failure(exception) =>
        throw InitializationException(
          "Unable to load points of interest.",
          exception
        )
      case Success(value) =>
        val amountOfPois = value.map(_._2.size).sum
        logger.info(
          s"Received {} POIs in {}s.\n\t{}",
          amountOfPois,
          "%.2f".format((System.currentTimeMillis() - start) / 1000d),
          value
            .map { case (k, v) =>
              s"$k -> ${v.size} (${"%.2f".format(v.size.doubleValue / amountOfPois * 100)} %)"
            }
            .mkString("\n\t")
        )
        value
    }
  }

  /** Create empiric probability density functions for the POIs of a type.
    *
    * @param pois
    *   All POIs in the area
    * @return
    *   Map with POIs and size for all POI types
    */
  def createPoiPdf(
      pois: Map[CategoricalLocationDictionary.Value, Set[PointOfInterest]]
  ): Map[CategoricalLocationDictionary.Value, ProbabilityDensityFunction[
    PointOfInterest
  ]] = {
    pois.map { case (poiType, setOfPOIs) =>
      (
        poiType,
        ProbabilityDensityFunction(
          SortedMap.empty[PointOfInterest, Double] ++ setOfPOIs
            .map { POI =>
              (POI, POI.size)
            }
            .toVector
            .sortBy { case (poi, _) =>
              poi.id
            }
        )
      )
    }
  }

  /* not used right now, could be removed later */
  def calculatePoiSizes(
      POIs: Map[Int, Set[PointOfInterest]]
  ): Map[Int, Double] = {
    POIs.map { case (poiType, setOfPOIs) =>
      (
        poiType,
        setOfPOIs.foldLeft(0.0)((sizeSum: Double, poi: PointOfInterest) => {
          sizeSum + poi.size
        })
      )
    }
  }
}
