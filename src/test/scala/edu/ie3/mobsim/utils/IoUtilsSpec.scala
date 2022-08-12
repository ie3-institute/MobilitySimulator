/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits.{
  KILOWATT,
  KILOWATTHOUR,
  KILOWATTHOUR_PER_KILOMETRE
}

import java.io.{BufferedReader, File, FileReader}
import java.util

class IoUtilsSpec extends UnitSpec with IoUtilsTestData {
  "IoUtils" should {
    "write movement correctly" in {
      ioUtils.writeMovement(
        firstEv,
        currentTime,
        status,
        uuid
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
        s"$currentTime;" +
        s"${firstEv.uuid};$status;" +
        s"${firstEv.storedEnergy.getValue.doubleValue() / firstEv.batteryCapacity.getValue
          .doubleValue()};" +
        s"${firstEv.destinationPoi.id};" +
        s"${firstEv.destinationPoi.categoricalLocation};" +
        s"${firstEv.departureTime};" +
        s"${firstEv.chargingAtSimona}"

      list.forEach { str =>
        if (str.contains(uuid.toString)) {
          str shouldBe compareString
        }
      }
    }

    "write evs correctly" in {
      ioUtils.writeEvs(evSet)

      val data = new BufferedReader(
        new FileReader(new File(outputFileDir, "evs.csv"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val firstEvString: String = s"${firstEv.uuid};" +
        s"${firstEv.id};" +
        s"${firstEv.model};" +
        s"${firstEv.storedEnergy.to(KILOWATTHOUR).getValue.doubleValue()};" +
        s"${firstEv.acChargingPower.to(KILOWATT).getValue.doubleValue()};" +
        s"${firstEv.dcChargingPower.to(KILOWATT).getValue.doubleValue()};" +
        s"${firstEv.consumption.to(KILOWATTHOUR_PER_KILOMETRE).getValue.doubleValue()};" +
        s"${firstEv.homePoi.id};" +
        s"${firstEv.workPoi.id};" +
        s"${firstEv.chargingAtHomePossible}"

      val secondEvString: String = s"${secondEv.uuid};" +
        s"${secondEv.id};" +
        s"${secondEv.model};" +
        s"${secondEv.storedEnergy.to(KILOWATTHOUR).getValue.doubleValue()};" +
        s"${secondEv.acChargingPower.to(KILOWATT).getValue.doubleValue()};" +
        s"${secondEv.dcChargingPower.to(KILOWATT).getValue.doubleValue()};" +
        s"${secondEv.consumption.to(KILOWATTHOUR_PER_KILOMETRE).getValue.doubleValue()};" +
        s"${secondEv.homePoi.id};" +
        s"${secondEv.workPoi.id};" +
        s"${secondEv.chargingAtHomePossible}"

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
        uuid
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
          .getOrElse(cs6.uuid, Set.empty)
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

      val compareString: String = s"${charging_hub_townPoi.id};" +
        s"${charging_hub_townPoi.getPoiType};" +
        s"${charging_hub_townPoi.size};" +
        s"$cs4;" +
        s"${0.0}"

      val str = list.get(list.size() - 1)
      val randomUuid = str.split(";")(0)

      str shouldBe randomUuid + ";" + compareString
    }

    "write positions correctly" in {
      ioUtils.writeEvPosition(firstEv, currentTime, uuid)

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
            firstEv.destinationPoi.toString
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
  }
}
