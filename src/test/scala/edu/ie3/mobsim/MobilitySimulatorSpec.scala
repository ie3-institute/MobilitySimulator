/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.test.common.UnitSpec

import java.util.UUID
import scala.collection.immutable.{SortedSet, TreeSet}

class MobilitySimulatorSpec extends UnitSpec with MobilitySimulatorTestData {
  "MobilitySimulator" should {

    "define movement correctly" in {
      val defineMovements =
        PrivateMethod[(SortedSet[ElectricVehicle], SortedSet[ElectricVehicle])](
          Symbol("defineMovements")
        )

      val cases = Table(
        ("parkingEvs", "departingEvs"),
        (
          evIsParking(Seq(ev1, ev2, ev3)),
          TreeSet.empty[ElectricVehicle]
        ),
        (
          evIsParking(Seq(ev1, ev2)),
          evIsDeparting(Seq(ev3))
        ),
        (
          evIsParking(Seq(ev1)),
          evIsDeparting(Seq(ev2, ev3))
        ),
        (
          TreeSet.empty[ElectricVehicle],
          evIsDeparting(Seq(ev1, ev2, ev3))
        )
      )

      forAll(cases) { (parkingEvs, departingEvs) =>
        val evs: SortedSet[ElectricVehicle] = parkingEvs ++ departingEvs

        val (resultParking, resultDeparting) =
          mobSim invokePrivate defineMovements(
            evs,
            givenSimulationStart
          )

        resultParking shouldBe parkingEvs
        resultDeparting shouldBe departingEvs
      }
    }

    "handle departures" in {
      val handleDepartures =
        PrivateMethod[Map[UUID, Integer]](Symbol("handleDepartures"))

      val resultingMap: Map[UUID, Integer] =
        mobSim invokePrivate handleDepartures(
          evIsDeparting(Seq(ev1, ev2, ev3)),
          chargingPointsAllTaken,
          builder
        )

      resultingMap shouldBe Map(
        cs6.getUuid -> Integer.valueOf(cs6.getChargingPoints)
      )
    }

  }
}
