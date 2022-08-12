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
        ("departingEvs", "expectedChargingPoints"),
        (SortedSet(ev1), 1),
        (SortedSet(ev1, ev2), 2),
        (SortedSet(ev1, ev2, ev3), 3)
      )

      forAll(cases) { (departingEvs, expectedCsCount) =>
        val actualChargingPoints =
          mobSim invokePrivate handleDepartures(
            setEvsAsDeparting(departingEvs),
            chargingPointsAllTaken,
            builder
          )

        actualChargingPoints shouldBe Map(
          cs6.uuid -> Integer.valueOf(expectedCsCount)
        )
      }
    }

    "handle departing evs" in {
      val handleDepartingEvs =
        PrivateMethod[(Map[UUID, Integer], Seq[Movement])](
          Symbol("handleDepartingEvs")
        )

      val cases = Table(
        ("evs", "expectedFreeCs"),
        (SortedSet(ev1), Map(cs6.uuid -> 1)),
        (SortedSet(ev1, ev2), Map(cs6.uuid -> 2)),
        (SortedSet(ev1, ev2, ev3), Map(cs6.uuid -> 3))
      )

      forAll(cases) { (evs, expectedFreeCs) =>
        val (actualFreeCs, actualMovements) =
          mobSim invokePrivate handleDepartingEvs(setEvsAsDeparting(evs))

        val expectedMovements = evs.toSeq.map { ev =>
          Movement(
            cs6.uuid,
            ev.copy(
              storedEnergy = half,
              destinationPoi = workPoi,
              parkingTimeStart = givenSimulationStart.plusHours(-4),
              departureTime = givenSimulationStart,
              chargingAtSimona = false,
              chosenChargingStation = None
            )
          )
        }

        actualFreeCs shouldBe expectedFreeCs
        actualMovements shouldBe expectedMovements
      }
    }

    "handle departing ev" in {
      val handleDepartingEv = PrivateMethod[Future[Option[Movement]]](
        Symbol("handleDepartingEv")
      )

      val cases = Table(
        ("ev", "expectedResults"),
        (
          evChargingAtSimonaWithStation,
          Some(
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

      forAll(cases) { (ev, expectedResults) =>
        val result =
          mobSim invokePrivate handleDepartingEv(ev)

        result.onComplete(
          {
            case Success(actualResults) =>
              actualResults match {
                case Some(Movement(cs, ev)) =>
                  cs shouldBe cs6.uuid

                  ev.chosenChargingStation shouldBe None
                  ev.chargingAtSimona shouldBe false
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

      forAll(cases) { (freeLots, change, expectedFreeLots) =>
        val actualFreeLots =
          mobSim invokePrivate updateFreeLots(freeLots, change)

        actualFreeLots shouldBe expectedFreeLots
      }
    }

    "handle parking evs" in {
      val handleParkingEvs = PrivateMethod[Seq[Movement]](
        Symbol("handleParkingEvs")
      )

      val cases = Table(
        "evs",
        setEvsAsParking(SortedSet(ev1)),
        setEvsAsParking(SortedSet(ev1, ev2)),
        setEvsAsParking(SortedSet(ev1, ev2, ev3))
      )

      forAll(cases) { evs =>
        val actualMovements = mobSim invokePrivate handleParkingEvs(
          evs,
          pricesAtChargingStation,
          chargingPointsAllFree,
          maxDistance
        )

        val expectedMovements: Seq[Movement] = evs.toSeq.map { ev =>
          Movement(
            cs6.uuid,
            ev.setChargingAtSimona().setChosenChargingStation(Some(cs6.uuid))
          )
        }

        actualMovements shouldBe expectedMovements
      }
    }

    "handle arriving ev" in {
      val handleArrivingEv = PrivateMethod[Option[Movement]](
        Symbol("handleArrivingEv")
      )

      val cases = Table(
        ("ev", "prices", "availablePoints", "expectedMovement"),
        (
          arrivingEv,
          Map(cs6.uuid -> 0.0),
          Map(cs6.uuid -> 1),
          Some(
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

      forAll(cases) { (ev, prices, availablePoints, expectedMovement) =>
        val actualMovement = mobSim invokePrivate handleArrivingEv(
          ev,
          prices,
          availablePoints,
          maxDistance
        )

        actualMovement shouldBe expectedMovement
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
        ("evs", "expectedNextEvent"),
        (Seq(arrivingEv).toSet, 3600),
        (Seq(ev2).toSet, 2700),
        (Seq(arrivingEv, ev2).toSet, 2700)
      )

      forAll(cases) { (evs, expectedNextEvent) =>
        val actualNextEvent = mobSim invokePrivate getTimeUntilNextEvent(
          evs,
          givenSimulationStart
        )

        actualNextEvent shouldBe expectedNextEvent
      }
    }
  }
}
