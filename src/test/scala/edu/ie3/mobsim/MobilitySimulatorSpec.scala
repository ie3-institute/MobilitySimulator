/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.MobilitySimulator.Movement
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.test.common.UnitSpec

import java.util.UUID
import scala.collection.immutable.SortedSet
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
          setEvsAsParking(SortedSet(ev1, ev2, ev3)),
          SortedSet.empty[ElectricVehicle]
        ),
        (
          setEvsAsParking(SortedSet(ev1, ev2)),
          setEvsAsDeparting(SortedSet(ev3))
        ),
        (
          setEvsAsParking(SortedSet(ev1)),
          setEvsAsDeparting(SortedSet(ev2, ev3))
        ),
        (
          SortedSet.empty[ElectricVehicle],
          setEvsAsDeparting(SortedSet(ev1, ev2, ev3))
        )
      )

      forAll(cases) { (parkingEvs, departingEvs) =>
        val evs = parkingEvs ++ departingEvs

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
        (SortedSet(ev1), 1),
        (SortedSet(ev1, ev2), 2),
        (SortedSet(ev1, ev2, ev3), 3)
      )

      forAll(cases) { (departingEvs, resultingChargingPoints) =>
        val mapWithFreePoints =
          mobSim invokePrivate handleDepartures(
            setEvsAsDeparting(departingEvs),
            chargingPointsAllTaken,
            builder
          )

        mapWithFreePoints shouldBe Map(
          cs6.uuid -> Integer.valueOf(resultingChargingPoints)
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
        (SortedSet(ev1), Map(cs6.uuid -> 1)),
        (SortedSet(ev1, ev2), Map(cs6.uuid -> 2)),
        (SortedSet(ev1, ev2, ev3), Map(cs6.uuid -> 3))
      )

      forAll(cases) { (evs, resultingMap) =>
        val departedEvs = setEvsAsDeparting(evs)

        val (map, sequence) =
          mobSim invokePrivate handleDepartingEvs(departedEvs)

        val resultingSequence = departedEvs.toSeq.map { ev =>
          Movement(
            cs6.uuid,
            ev.removeChargingAtSimona().setChosenChargingStation(None)
          )
        }

        map shouldBe resultingMap
        sequence shouldBe resultingSequence
      }
    }

    "handle departing ev" in {
      val handleDepartingEv = PrivateMethod[Future[Option[Movement]]](
        Symbol("handleDepartingEv")
      )

      val cases = Table(
        ("ev", "option"),
        (
          evChargingAtSimonaWithStation,
          Option(
            Movement(
              cs6.uuid,
              evChargingAtSimonaWithStation
                .removeChargingAtSimona()
                .setChosenChargingStation(None)
            )
          )
        ),
        (evChargingAtSimonaWithoutStation, None)
      )

      forAll(cases) { (ev, option) =>
        val result =
          mobSim invokePrivate handleDepartingEv(ev)

        result.onComplete(
          {
            case Success(value) =>
              value shouldBe option

              value match {
                case Some(value) =>
                  val resultingEv: ElectricVehicle = value.ev
                  resultingEv.chosenChargingStation shouldBe None
                  resultingEv.chargingAtSimona shouldBe false
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
          Map(cs6.uuid -> 0, cs5.uuid -> 0),
          Map(cs6.uuid -> 1),
          Map(cs6.uuid -> 1, cs5.uuid -> 0)
        ),
        (
          Map(cs6.uuid -> 0, cs5.uuid -> 0),
          Map(cs6.uuid -> 2, cs5.uuid -> 1),
          Map(cs6.uuid -> 2, cs5.uuid -> 1)
        )
      )

      forAll(cases) { (freeLots, change, resultingMap) =>
        val map =
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
        (SortedSet(ev1), Map(cs6.uuid -> -1)),
        (SortedSet(ev1, ev2), Map(cs6.uuid -> -2)),
        (SortedSet(ev1, ev2, ev3), Map(cs6.uuid -> -3))
      )

      forAll(cases) { (evs, resultingMap) =>
        val parkingEvs = setEvsAsParking(evs)

        val (map, sequence) = mobSim invokePrivate handleParkingEvs(
          parkingEvs,
          pricesAtChargingStation,
          chargingPointsAllFree,
          maxDistance
        )

        val resultingSequence: Seq[Movement] = parkingEvs.toSeq.map { ev =>
          Movement(
            cs6.uuid,
            ev.setChargingAtSimona()
              .setChosenChargingStation(Some(cs6.uuid))
          )
        }

        map shouldBe resultingMap
        sequence shouldBe resultingSequence
      }
    }

    "handle arriving ev" in {
      val handleArrivingEv = PrivateMethod[Future[Option[Movement]]](
        Symbol("handleArrivingEv")
      )

      val cases = Table(
        ("ev", "prices", "availablePoints", "option"),
        (
          arrivingEv,
          Map(cs6.uuid -> 0.0),
          Map(cs6.uuid -> 1),
          Option(
            Movement(
              cs6.uuid,
              arrivingEv
                .setChargingAtSimona()
                .setChosenChargingStation(Some(cs6.uuid))
            )
          )
        ),
        (
          arrivingEv,
          Map(cs6.uuid -> 0.0),
          Map(cs6.uuid -> 0),
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
                  val resultingEv: ElectricVehicle = value.ev
                  resultingEv.chosenChargingStation shouldBe Some(
                    cs6.uuid
                  )
                  resultingEv.chargingAtSimona shouldBe true
                case None => None
              }
            case Failure(exception) => throw exception
          }
        )
      }
    }

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
        val nextEvent = mobSim invokePrivate getTimeUntilNextEvent(
          evs,
          givenSimulationStart
        )

        nextEvent shouldBe result
      }
    }
  }
}
