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
import scala.jdk.CollectionConverters._

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
        new FileReader(new File(outputFileFolder + "movements"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val compareString: String = (s"${uuid.toString}," +
        s"${currentTime.toString}," +
        s"${firstEv.uuid.toString},$status," +
        s"${(firstEv.storedEnergy.getValue.doubleValue() / firstEv.batteryCapacity.getValue
          .doubleValue()).toString}," +
        s"${firstEv.destinationPoi.id}," +
        s"${firstEv.destinationPoi.categoricalLocation.toString}," +
        s"${firstEv.departureTime.toString}," +
        s"${firstEv.chargingAtSimona.toString}").replaceAll(",", ";")

      list.get(list.size() - 2) shouldBe compareString
    }

    "write evs correctly" in {
      ioUtils.writeEvs(evSet)

      val data = new BufferedReader(
        new FileReader(new File(outputFileFolder + "evs"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val firstEvString: String = (s"${firstEv.uuid.toString}," +
        s"${firstEv.id}," +
        s"${firstEv.model}," +
        s"${firstEv.storedEnergy.to(KILOWATTHOUR).getValue.doubleValue().toString}," +
        s"${firstEv.acChargingPower.to(KILOWATT).getValue.doubleValue().toString}," +
        s"${firstEv.dcChargingPower.to(KILOWATT).getValue.doubleValue().toString}," +
        s"${firstEv.consumption.to(KILOWATTHOUR_PER_KILOMETRE).getValue.doubleValue().toString}," +
        s"${firstEv.homePoi.id}," +
        s"${firstEv.workPoi.id}," +
        s"${firstEv.chargingAtHomePossible.toString}").replaceAll(",", ";")

      val secondEvString: String = (s"${secondEv.uuid.toString}," +
        s"${secondEv.id}," +
        s"${secondEv.model}," +
        s"${secondEv.storedEnergy.to(KILOWATTHOUR).getValue.doubleValue().toString}," +
        s"${secondEv.acChargingPower.to(KILOWATT).getValue.doubleValue().toString}," +
        s"${secondEv.dcChargingPower.to(KILOWATT).getValue.doubleValue().toString}," +
        s"${secondEv.consumption.to(KILOWATTHOUR_PER_KILOMETRE).getValue.doubleValue().toString}," +
        s"${secondEv.homePoi.id}," +
        s"${secondEv.workPoi.id}," +
        s"${secondEv.chargingAtHomePossible.toString}").replaceAll(",", ";")

      list.get(list.size() - 2) shouldBe firstEvString
      list.get(list.size() - 1) shouldBe secondEvString
    }

    "write electric vehicle charging stations correctly" in {
      ioUtils.writeEvcs(
        cs6,
        currentlyAvailableChargingPoints,
        currentTime,
        uuid
      )

      val data = new BufferedReader(
        new FileReader(new File(outputFileFolder + "evcs"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val chargingPoints: String = cs6.getChargingPoints.toString
      val occupiedChargingPoints: String =
        (cs6.getChargingPoints - availableChargingPoints.getOrElse(
          cs6.getUuid,
          0
        )).toString

      val entry: String = (s"${uuid.toString}," +
        s"${currentTime.toString}," +
        s"${cs6.getUuid.toString}," +
        s"$chargingPoints," +
        s"$occupiedChargingPoints").replaceAll(",", ";")

      list.get(list.size() - 1) shouldBe entry
    }

    "write pois correctly" in {
      ioUtils.writePois(poiMap)

      val data = new BufferedReader(
        new FileReader(new File(outputFileFolder + "pois"))
      )

      val list: util.ArrayList[String] = new util.ArrayList[String]()
      var line = data.readLine()

      while (line != null) {
        list.add(line)
        line = data.readLine()
      }

      val string: String =
        charging_hub_townPoi.nearestChargingStations.foreach {
          case (evcsUuid, distance) =>
            evcsUuid.toString + "," + distance.getValue.doubleValue().toString
        }.toString

      val entry: String = (s"${charging_hub_townPoi.id}," +
        s"${charging_hub_townPoi.getPoiType.toString}," +
        s"${charging_hub_townPoi.size.toString}," +
        s"$string").replaceAll(",", ";")

      list.get(list.size() - 1) contains entry
    }

    "write positions correctly" in {
      ioUtils.writeEvPosition(firstEv, currentTime, uuid)

      val data = new BufferedReader(
        new FileReader(new File(outputFileFolder + "positions"))
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

      val entry: String = (s"$uuid," +
        s"${firstEv.uuid.toString}," +
        s"$location,").replaceAll(",", ";") +
        s"$destinationPoi"

      list.get(list.size() - 1) shouldBe entry
    }
  }
}
