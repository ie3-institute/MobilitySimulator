/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.test.common.UnitSpec

import scala.collection.immutable.SortedSet

class MobilitySimulatorSpec extends UnitSpec with MobilitySimulatorTestData {
  "MobilitySimulator" should {
    "do activity correctly" in {
      // val nextTick = mobSim.doActivity(0L)
    }

    "define movement correctly" in {
      val defineMovements =
        PrivateMethod[(SortedSet[ElectricVehicle], SortedSet[ElectricVehicle])](
          Symbol("defineMovements")
        )

      val cases = Table(
        ("evs","parkingEvs","departingEvs"),
        (Seq(ev1parking,ev2parking),Seq(ev1parking,ev2parking),Seq.empty[ElectricVehicle]),
        (Seq(ev1parking,ev2departing),Seq(ev1parking),Seq(ev2departing))
      )


      forAll(cases) { (evs, parkingEvs, departingEvs) =>
        val evSet: SortedSet[ElectricVehicle] = SortedSet.empty[ElectricVehicle] ++ evs.toSet

        MobilitySimulator invokePrivate defineMovements(
          evSet,
          givenSimulationStart
        ) shouldBe (parkingEvs.toSet,departingEvs.toSet)
      }
    }

  }

}
