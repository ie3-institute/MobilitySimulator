/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.io.geodata.PoiTestData
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.mobsim.utils.IoUtilsSpec.evString
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR
import squants.space.Kilometers

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Path
import java.util
import java.util.UUID

class IoUtilsSpec extends UnitSpec with IoUtilsTestData with PoiTestData {
  "IoUtils" should {
    "write movement correctly" in {
      val time = currentTime
      ioUtils.writeMovement(
        firstEv,
        time,
      )

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "movements.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      list.size shouldBe 2

      list
        .stream()
        .skip(1)
        .findFirst()
        .ifPresent { str =>
          val parts = str.split(";")
          parts(0) shouldBe firstEv.uuid.toString
          parts(1) shouldBe time.toString
          parts(2) shouldBe "1.0"
          parts(3) shouldBe "test"
          parts(4) shouldBe CategoricalLocationDictionary.HOME.toString
          parts(5) shouldBe CategoricalLocationDictionary.HOME.toString
          parts(6) shouldBe firstEv.parkingTimeStart.toString
          parts(7) shouldBe firstEv.departureTime.toString
        }
    }

    "write evs correctly" in {
      ioUtils.writeEvs(evs)

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "evs.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val firstEvString: String = evString(firstEv)

      val secondEvString: String = evString(secondEv)

      list.forEach { str =>
        if (str.contains(firstEv.uuid.toString)) {
          str shouldBe firstEvString
        }
        if (str.contains(secondEv.uuid.toString)) {
          str shouldBe secondEvString
        }
      }
    }

    "write pois correctly" in {
      val homePoiUuid = UUID.randomUUID()
      val evcsUuid = UUID.randomUUID()
      val directHomeMapping = Map(homePoiUuid -> evcsUuid)

      val poiMap = Map(
        CategoricalLocationDictionary.HOME -> Set(
          homePoi,
          homePoiWithoutNearestCharger,
        ),
        CategoricalLocationDictionary.WORK -> Set(workPoi),
      )

      ioUtils.writePois(poiMap, directHomeMapping)

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "pois.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      list.size() shouldBe 4

      list
        .stream()
        .filter(entry => entry.contains(homePoi.id))
        .findFirst()
        .ifPresent { str =>
          val parts = str.split(";")
          parts(1) shouldBe homePoi.id
          parts(2) shouldBe CategoricalLocationDictionary.HOME.toString
          parts(3) shouldBe homePoi.size.toString
          parts(4).split(",")(0) shouldBe
            ("ChargingStation(" + cs1.uuid.toString)
          parts(5) shouldBe "0.0"
        }

      list
        .stream()
        .filter(entry => entry.contains(homePoiWithoutNearestCharger.id))
        .findFirst()
        .ifPresent { str =>
          val parts = str.split(";")
          parts(1) shouldBe homePoiWithoutNearestCharger.id
          parts(2) shouldBe CategoricalLocationDictionary.HOME.toString
          parts(3) shouldBe homePoiWithoutNearestCharger.size.toString
          parts(4) shouldBe ""
          parts(5) shouldBe Kilometers(0).toString
        }

      val workEntries = list
        .stream()
        .filter(entry => entry.contains(workPoi.id))
        .toArray
        .map(_.toString)

      workEntries.length shouldBe 1

      workPoi.nearestChargingStations.foreach { case (stationUuid, distance) =>
        val foundEntry =
          workEntries.find(entry => entry.contains(stationUuid.toString))
        foundEntry shouldBe defined

        foundEntry.foreach { entry =>
          val parts = entry.split(";")
          parts(1) shouldBe workPoi.id
          parts(2) shouldBe CategoricalLocationDictionary.WORK.toString
          parts(3) shouldBe workPoi.size.toString
          parts(4) shouldBe stationUuid.toString
          parts(5) shouldBe distance.toKilometers.toString
        }
      }
    }

    "write positions correctly" in {
      ioUtils.writeEvPosition(firstEv, currentTime)

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "positions.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      list.size() shouldBe 2

      list
        .stream()
        .skip(1)
        .findFirst()
        .ifPresent { str =>
          val parts = str.split(";")
          parts(0) shouldBe currentTime.toString
          parts(1) shouldBe firstEv.uuid.toString
          parts(2) shouldBe firstEv.destinationPoiType.toString
          parts(3) shouldBe firstEv.destinationPoi.toString
        }
    }

    "read ev inputs successfully" in {
      val path = Path.of(getClass.getResource("ev_input_data").toURI)
      val csvParams = CsvParams(
        ",",
        path.toString,
      )
      val evInputs = IoUtils.readEvInputs(csvParams)
      evInputs should have size 1
    }
  }
}
object IoUtilsSpec {

  def evString(ev: ElectricVehicle): String = {
    s"${ev.uuid};" +
      s"${ev.id};" +
      s"${ev.evType.model};" +
      s"${ev.storedEnergy.to(KILOWATTHOUR).getValue.doubleValue()};" +
      s"${ev.evType.acPower.toKilowatts};" +
      s"${ev.evType.dcPower.toKilowatts};" +
      s"${ev.evType.consumption.toKilowattHoursPerKilometer};" +
      s"${ev.homePoi.id};" +
      s"${ev.workPoi.id};" +
      s"${ev.chargingAtHomePossible}"
  }
}
