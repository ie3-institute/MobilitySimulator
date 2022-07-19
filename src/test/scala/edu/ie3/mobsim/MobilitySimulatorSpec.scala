/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.MobilitySimulator.{Movement, seed}
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.test.common.UnitSpec

import java.util.UUID
import scala.collection.immutable.{SortedSet, TreeSet}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Success}

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

      val cases = Table(
        ("departingEvs", "resultingChargingPoints"),
        (Seq(ev1), 1),
        (Seq(ev1, ev2), 2),
        (Seq(ev1, ev2, ev3), 3)
      )

      forAll(cases) { (departingEvs, resultingChargingPoints) =>
        val mapWithFreePoints: Map[UUID, Integer] =
          mobSim invokePrivate handleDepartures(
            evIsDeparting(departingEvs),
            chargingPointsAllTaken,
            builder
          )

        mapWithFreePoints shouldBe Map(
          cs6.getUuid -> Integer.valueOf(resultingChargingPoints)
        )
      }
    }

    "handle departing evs" in {
      val handleDepartingEvs =
        PrivateMethod[(Map[UUID, Integer], Seq[Movement])](
          Symbol("handleDepartingEvs")
        )

      val cases = Table(
        ("evs", "resultingMap"),
        (Seq(ev1), Map(cs6.getUuid -> 1)),
        (Seq(ev1, ev2), Map(cs6.getUuid -> 2)),
        (Seq(ev1, ev2, ev3), Map(cs6.getUuid -> 3))
      )

      forAll(cases) { (evs, resultingMap) =>
        val (map, sequence) =
          mobSim invokePrivate handleDepartingEvs(evIsDeparting(evs))

        val resultingSequence: Seq[Movement] = evs.map { ev =>
          Movement(cs6.getUuid, ev)
        }

        map shouldBe resultingMap
        sequence shouldBe resultingSequence
      }
    }

    "handle departing ev" in {
      val handleDepartingEv = PrivateMethod[Future[Option[(UUID, Movement)]]](
        Symbol("handleDepartingEv")
      )

      val cases = Table(
        ("ev", "option"),
        (
          evChargingAtSimonaWithStation,
          Option(
            cs6.getUuid,
            Movement(cs6.getUuid, evChargingAtSimonaWithStation)
          )
        ),
        (evChargingAtSimonaWithoutStation, None)
      )

      forAll(cases) { (ev, option) =>
        val result: Future[Option[(UUID, Movement)]] =
          mobSim invokePrivate handleDepartingEv(ev)

        result.onComplete(
          {
            case Success(value) =>
              value shouldBe option

              value match {
                case Some(value) =>
                  val resultingEv: ElectricVehicle = value._2.ev
                  resultingEv.getChosenChargingStation shouldBe None
                  resultingEv.isChargingAtSimona shouldBe false
                case None => None
              }
            case Failure(exception) => throw exception
          }
        )
      }
    }

    "update free lots" in {
      val updateFreeLots =
        PrivateMethod[Map[UUID, Integer]](Symbol("updateFreeLots"))

      val cases = Table(
        ("freeLots", "change", "resultingMap"),
        (
          Map(cs6.getUuid -> 0, cs5.getUuid -> 0),
          Map(cs6.getUuid -> 1),
          Map(cs6.getUuid -> 1, cs5.getUuid -> 0)
        ),
        (
          Map(cs6.getUuid -> 0, cs5.getUuid -> 0),
          Map(cs6.getUuid -> 2, cs5.getUuid -> 1),
          Map(cs6.getUuid -> 2, cs5.getUuid -> 1)
        )
      )

      forAll(cases) { (freeLots, change, resultingMap) =>
        val map: Map[UUID, Integer] =
          mobSim invokePrivate updateFreeLots(freeLots, change)

        map shouldBe resultingMap
      }
    }

    "handle parking evs" in {
      val handleParkingEvs = PrivateMethod[(Map[UUID, Integer], Seq[Movement])](
        Symbol("handleParkingEvs")
      )

      val cases = Table(
        ("evs", "resultingMap"),
        (evIsParking(Seq(ev1)), Map(cs6.getUuid -> -1)),
        (evIsParking(Seq(ev1, ev2)), Map(cs6.getUuid -> -2)),
        (evIsParking(Seq(ev1, ev2, ev3)), Map(cs6.getUuid -> -3))
      )

      forAll(cases) { (evs, resultingMap) =>
        val (map, sequence) = mobSim invokePrivate handleParkingEvs(
          evs,
          pricesAtChargingStation,
          chargingPointsAllFree,
          maxDistance
        )

        val resultingSequence: Seq[Movement] = evs.toSeq.map { ev =>
          Movement(cs6.getUuid, ev)
        }

        map shouldBe resultingMap
        sequence shouldBe resultingSequence
      }
    }

    "handle arriving ev" in {
      val handleArrivingEv = PrivateMethod[Future[Option[(UUID, Movement)]]](
        Symbol("handleArrivingEv")
      )

      val cases = Table(
        ("ev", "prices", "availablePoints", "option"),
        (
          arrivingEv,
          Map(cs6.getUuid -> 0.0),
          Map(cs6.getUuid -> 1),
          Option(cs6.getUuid, Movement(cs6.getUuid, arrivingEv))
        ),
        (
          arrivingEv,
          Map(cs6.getUuid -> 0.0),
          Map(cs6.getUuid -> 0),
          None
        ),
        (
          arrivingEv,
          Map.empty[UUID, Double],
          Map.empty[UUID, Integer],
          None
        )
      )

      forAll(cases) { (ev, prices, availablePoints, option) =>
        val result = mobSim invokePrivate handleArrivingEv(
          ev,
          prices,
          availablePoints,
          maxDistance
        )

        result.onComplete(
          {
            case Success(value) =>
              value shouldBe option

              value match {
                case Some(value) =>
                  val resultingEv: ElectricVehicle = value._2.ev
                  resultingEv.getChosenChargingStation shouldBe Some(
                    cs6.getUuid
                  )
                  resultingEv.isChargingAtSimona shouldBe true
                case None => None
              }
            case Failure(exception) => throw exception
          }
        )
      }
    }

    "update and simulate departed evs" in {}

    "get time until next event" in {
      val getTimeUntilNextEvent =
        PrivateMethod[Long](Symbol("getTimeUntilNextEvent"))

      val ev2: ElectricVehicle =
        evChargingAtSimonaWithStation.copy(departureTime =
          givenSimulationStart.plusMinutes(45)
        )

      val cases = Table(
        ("evs", "result"),
        (Seq(arrivingEv).toSet, 3600),
        (Seq(ev2).toSet, 2700),
        (Seq(arrivingEv, ev2).toSet, 2700)
      )

      forAll(cases) { (evs, result) =>
        val nextEvent: Long = mobSim invokePrivate getTimeUntilNextEvent(
          evs,
          givenSimulationStart
        )

        nextEvent shouldBe result
      }
    }
  }
}
