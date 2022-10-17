/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.simona.api.data.ev.model.EvModel
import edu.ie3.test.common.UnitSpec

import java.util.UUID
import scala.collection.immutable.Seq
import scala.jdk.CollectionConverters.{MapHasAsJava, SeqHasAsJava}

class MovementSpec extends UnitSpec with TripSimulationTestData {

  "Movement" should {
    val csUuidA = UUID.randomUUID()
    val csUuidB = UUID.randomUUID()
    val movements = Seq(
      Movement(csUuidA, ev1),
      Movement(csUuidB, ev2),
      Movement(csUuidB, ev3)
    )

    "build ev model maps correctly" in {
      val actualMap = Movement.buildMovementsMap(movements)
      val expectedMap = Map(
        csUuidA -> Seq(ev1.asInstanceOf[EvModel]).asJava,
        csUuidB -> Seq(
          ev2.asInstanceOf[EvModel],
          ev3.asInstanceOf[EvModel]
        ).asJava
      ).asJava
      actualMap shouldBe expectedMap
    }

    "build ev uuid maps correctly" in {
      val actualMap = Movement.buildMovementsUuidMap(movements)
      val expectedMap = Map(
        csUuidA -> Seq(ev1.uuid).asJava,
        csUuidB -> Seq(
          ev2.uuid,
          ev3.uuid
        ).asJava
      ).asJava
      actualMap shouldBe expectedMap
    }
  }

}
