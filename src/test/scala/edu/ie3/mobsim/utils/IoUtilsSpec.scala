/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.mobsim.utils.IoUtilsSpec.evString
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits.KILOWATTHOUR

import java.io.{BufferedReader, File, FileReader}
import java.nio.file.Path
import java.util

class IoUtilsSpec extends UnitSpec with IoUtilsTestData {
  "IoUtils" should {
    "write movement correctly" in {
      ioUtils.writeMovement(
        firstEv,
        currentTime,
        status,
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

      val compareString: String = s"$uuid;" +
        s"${firstEv.uuid};" +
        s"$status;" +
        s"${firstEv.getStoredEnergy
            .divide(firstEv.getEStorage)
            .getValue
            .doubleValue()
            .toString};" +
        s"${firstEv.destinationPoi.id};" +
        s"${firstEv.destinationPoiType.toString};" +
        s"${firstEv.destinationPoi.categoricalLocation};" +
        s"${firstEv.parkingTimeStart.toString};" +
        s"${firstEv.departureTime};" +
        s"${firstEv.chargingAtSimona}"

      list.forEach { str =>
        if (str.contains(uuid.toString)) {
          str shouldBe compareString
        }
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

    "write electric vehicle charging stations correctly" in {
      ioUtils.writeEvcs(
        cs6,
        chargingStationOccupancy,
        currentTime,
      )

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "evcs.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val chargingPoints: Int = cs6.chargingPoints
      val chargingEvs: String =
        chargingStationOccupancy
          .getOrElse(cs6.uuid, Seq.empty)
          .map(_.uuid)
          .mkString("[", "|", "]")

      val entry: String = s"$uuid;" +
        s"$currentTime;" +
        s"${cs6.uuid};" +
        s"$chargingPoints;" +
        s"$chargingEvs"

      list.forEach { str =>
        if (str.contains(uuid.toString)) {
          str shouldBe entry
        }
      }
    }

    "write pois correctly" in {
      ioUtils.writePois(poiMap)

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "pois.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val compareString: String = s"${chargingHubTownPoi.id};" +
        s"${PoiTypeDictionary.CHARGING_HUB_TOWN};" +
        s"${chargingHubTownPoi.size};" +
        s"$cs4;" +
        s"${0.0}"

      val str = list.get(list.size() - 1)
      val randomUuid = str.split(";")(0)

      str shouldBe randomUuid + ";" + compareString
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

      val (location, destinationPoi) =
        if (currentTime.isBefore(firstEv.parkingTimeStart)) {
          ("DRIVING", "")
        } else {
          (
            firstEv.destinationPoi.categoricalLocation.toString,
            firstEv.destinationPoi.toString,
          )
        }

      val compareString: String = s"$uuid;" +
        s"${firstEv.uuid.toString};" +
        s"$location;" +
        s"$destinationPoi"

      list.forEach { str =>
        if (str.contains(uuid.toString)) {
          str shouldBe compareString
        }
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
