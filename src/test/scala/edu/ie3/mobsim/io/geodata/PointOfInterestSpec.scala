/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.exceptions.InitializationException
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.model.ChargingStation
import edu.ie3.test.common.UnitSpec
import org.locationtech.jts.geom.Coordinate
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.util.UUID
import javax.measure.quantity.Length
import scala.util.{Failure, Success}

class PointOfInterestSpec extends UnitSpec with PoiTestData {
  "Creating a point of interest" when {
    val coordinate = new Coordinate(7.4116481, 51.4843282)
    val locationToChargingStations =
      Seq(cs0, cs1, cs2, cs3).groupBy(_.evcsLocationType)

    "assess the headline of a file correctly" should {
      val assessHeadLine =
        PrivateMethod[Map[String, Int]](Symbol("assessHeadLine"))

      "work fine, if the headline is okay" in {
        val header =
          Array("uuid", "id", "size", "lon", "lat", "categoricallocation")
        PointOfInterest invokePrivate assessHeadLine(header) shouldBe Map(
          "uuid" -> 0,
          "id" -> 1,
          "size" -> 2,
          "lon" -> 3,
          "lat" -> 4,
          "categoricallocation" -> 5
        )
      }

      "fail, if there are too few entries" in {
        val header = Array("uuid", "id", "size", "lon", "lat")
        val error = intercept[InitializationException] {
          PointOfInterest invokePrivate assessHeadLine(header)
        }
        error.msg shouldBe "Unable to read POI from file. Expected headline elements: 'uuid,id,size,lat,lon,categoricallocation', given elements: 'uuid,id,size,lon,lat'"
      }

      "fail, if an entry is missing" in {
        val header =
          Array("uuid", "id", "size", "lon", "lat", "somerandomeasteregg")
        val error = intercept[InitializationException] {
          PointOfInterest invokePrivate assessHeadLine(header)
        }
        error.msg shouldBe "Unable to obtain position of column 'categoricallocation'"
      }
    }

    "prepare the charging stations" should {
      val prepareChargingStations =
        PrivateMethod[Map[EvcsLocationType, Seq[ChargingStation]]](
          Symbol("prepareChargingStations")
        )

      "filter out unsuitable assets" in {
        val actual = PointOfInterest invokePrivate prepareChargingStations(
          Seq(cs0, cs1, cs2, cs3, cs4, cs5)
        )

        actual.keys should contain theSameElementsAs Seq(
          EvcsLocationType.HOME,
          EvcsLocationType.WORK,
          EvcsLocationType.STREET
        )
        /* Check, that an already assigned home-cs is not considered */
        actual.get(EvcsLocationType.HOME) match {
          case Some(cs) =>
            cs should contain theSameElementsAs Seq(cs0, cs1, cs5)
          case None => fail("Unable to determine cs of type home")
        }
      }
    }

    "parsing the content of the line" should {
      val parse = PrivateMethod[
        (
            UUID,
            String,
            Double,
            Coordinate,
            PoiEnums.CategoricalLocationDictionary.Value
        )
      ](Symbol("parse"))

      "deliver proper results" in {
        val expectedUuid =
          UUID.fromString("cfcbd423-eb38-49c5-9407-4370a0957391")
        val entries =
          Array(
            expectedUuid.toString,
            "test_poi",
            "3.4",
            "51.4843282",
            "7.4116481",
            "home"
          )
        val col2idx = Map(
          "uuid" -> 0,
          "id" -> 1,
          "size" -> 2,
          "lat" -> 3,
          "lon" -> 4,
          "categoricallocation" -> 5
        )

        PointOfInterest invokePrivate parse(entries, col2idx) match {
          case (uuid, id, size, coord, categoricalLocation) =>
            uuid shouldBe expectedUuid
            id shouldBe "test_poi"
            size shouldBe 3.4
            coord shouldBe coordinate
            categoricalLocation shouldBe CategoricalLocationDictionary.HOME
        }
      }

      "properly reflects the order of columns" in {
        val expectedUuid =
          UUID.fromString("e168b974-afd8-4683-bea4-bc609c417b80")
        val entries =
          Array(
            expectedUuid.toString,
            "test_poi",
            "3.4",
            "7.4116481",
            "51.4843282",
            "home"
          )
        val col2idx = Map(
          "uuid" -> 0,
          "id" -> 1,
          "size" -> 2,
          "lon" -> 3,
          "lat" -> 4,
          "categoricallocation" -> 5
        )

        PointOfInterest invokePrivate parse(entries, col2idx) match {
          case (uuid, id, size, coord, categoricalLocation) =>
            uuid shouldBe expectedUuid
            id shouldBe "test_poi"
            size shouldBe 3.4
            coord shouldBe coordinate
            categoricalLocation shouldBe CategoricalLocationDictionary.HOME
        }
      }
    }

    "finding suitable charging stations for categorical location types" should {
      val suitableChargingStations =
        PrivateMethod[Seq[ChargingStation]](Symbol("suitableChargingStations"))

      "find the right charging stations for categorical location 'home'" in {
        PointOfInterest invokePrivate suitableChargingStations(
          CategoricalLocationDictionary.HOME,
          locationToChargingStations
        ) should contain allElementsOf Seq(cs0, cs1)
      }

      "find the right charging stations for categorical location 'work'" in {
        PointOfInterest invokePrivate suitableChargingStations(
          CategoricalLocationDictionary.WORK,
          locationToChargingStations
        ) should contain allElementsOf Seq(cs3)
      }

      "find the right charging stations for other categorical locations" in {
        PointOfInterest invokePrivate suitableChargingStations(
          CategoricalLocationDictionary.SPORTS,
          locationToChargingStations
        ) should contain allElementsOf Seq(cs2)
      }
    }

    "determining the nearest charging stations" should {
      val nearbyChargingStations =
        PrivateMethod[Seq[(ChargingStation, ComparableQuantity[Length])]](
          Symbol("nearbyChargingStations")
        )

      "find the nearest ones with correct distance" in {
        val actual = PointOfInterest invokePrivate nearbyChargingStations(
          Seq(cs0, cs1, cs2),
          coordinate,
          Quantities.getQuantity(1000d, Units.METRE)
        )
        val expected = Map(
          cs0 -> Quantities
            .getQuantity(0.013113941716235453974464, Units.METRE),
          cs1 -> Quantities
            .getQuantity(1.10382753815854878670116, Units.METRE),
          cs2 -> Quantities
            .getQuantity(111.545809977148284216795, Units.METRE)
        )

        actual.toMap.keys should contain allElementsOf Seq(cs0, cs1, cs2)
        Seq(cs0, cs1, cs2).foreach { cs =>
          actual.toMap.get(cs).zip(expected.get(cs)) match {
            case Some((actual, expected)) =>
              actual should equalWithTolerance(expected)
            case None => fail("Unable to determine the expected distance")
          }
        }
      }

      "filter out cs, that are too far away" in {
        val actual = PointOfInterest invokePrivate nearbyChargingStations(
          Seq(cs0, cs1, cs2),
          coordinate,
          Quantities.getQuantity(100d, Units.METRE)
        )

        actual.toMap.keys should contain allElementsOf Seq(cs0, cs1)
      }
    }
  }

  "Parsing a whole csv" should {
    "provide correct results" in {
      val testFileName = this.getClass.getResource("poi.csv").getFile
      PointOfInterest.getFromFile(
        testFileName,
        ",",
        Seq(cs1, cs3),
        Quantities.getQuantity(50d, Units.METRE),
        Quantities.getQuantity(30d, Units.METRE)
      ) match {
        case Failure(exception) =>
          fail("Getting points of interest from file did fail.", exception)
        case Success(pois) =>
          pois should contain theSameElementsAs itData
      }
    }
  }
}
