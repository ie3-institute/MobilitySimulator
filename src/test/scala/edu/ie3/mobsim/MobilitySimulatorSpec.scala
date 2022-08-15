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

class MobilitySimulatorSpec extends UnitSpec with MobilitySimulatorTestData {
  "MobilitySimulator" should {

    "define movement correctly" in {
      val mobilitySimulator = mobSim()

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
          mobilitySimulator invokePrivate defineMovements(
            evs,
            givenSimulationStart
          )

        resultParking shouldBe parkingEvs
        resultDeparting shouldBe departingEvs
      }
    }

    "handle departures" in {
      val mobilitySimulator = mobSim()

      val handleDepartures =
        PrivateMethod[Map[UUID, Int]](Symbol("handleDepartures"))

      val cases = Table(
        ("departingEvs", "expectedChargingPoints"),
        (SortedSet(ev1), 1),
        (SortedSet(ev1, ev2), 2),
        (SortedSet(ev1, ev2, ev3), 3)
      )

      forAll(cases) { (departingEvs, expectedCsCount) =>
        val actualChargingPoints =
          mobilitySimulator invokePrivate handleDepartures(
            setEvsAsDeparting(departingEvs),
            chargingPointsAllTaken,
            builder
          )

        actualChargingPoints shouldBe Map(
          cs6.uuid -> expectedCsCount
        )
      }
    }

    "handle departing evs" in {
      val mobilitySimulator = mobSim()

      val handleDepartingEvs =
        PrivateMethod[(Map[UUID, Int], Seq[Movement])](
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
          mobilitySimulator invokePrivate handleDepartingEvs(
            setEvsAsDeparting(evs)
          )

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
        actualMovements.size shouldBe expectedMovements.size
        actualMovements should contain allElementsOf expectedMovements

        mobilitySimulator.electricVehicles.foreach { electricVehicle =>
          expectedMovements.foreach { movement =>
            if (movement.ev.uuid.equals(electricVehicle.uuid)) {
              electricVehicle shouldBe movement.ev
            }
          }
        }
      }
    }

    "handle departing ev" in {
      val mobilitySimulator = mobSim()

      val handleDepartingEv = PrivateMethod[Option[Movement]](
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
        val actualResults =
          mobilitySimulator invokePrivate handleDepartingEv(ev)

        actualResults shouldBe expectedResults

        actualResults match {
          case Some(Movement(cs, ev)) =>
            cs shouldBe cs6.uuid

            ev.chosenChargingStation shouldBe None
          case None =>
        }
      }
    }

    "update free lots" in {
      val mobilitySimulator = mobSim()

      val updateFreeLots =
        PrivateMethod[Map[UUID, Int]](Symbol("updateFreeLots"))

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
          mobilitySimulator invokePrivate updateFreeLots(freeLots, change)

        actualFreeLots shouldBe expectedFreeLots
      }
    }

    "handle parking evs" in {
      val mobilitySimulator = mobSim()

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
        val actualMovements = mobilitySimulator invokePrivate handleParkingEvs(
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

        mobilitySimulator.electricVehicles.foreach { electricVehicle =>
          expectedMovements.foreach { movement =>
            if (movement.ev.uuid.equals(electricVehicle.uuid)) {
              electricVehicle shouldBe movement.ev
            }
          }
        }
      }
    }

    "handle arriving ev" in {
      val mobilitySimulator = mobSim()

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
          Map.empty[UUID, Int],
          None
        )
      )

      forAll(cases) { (ev, prices, availablePoints, expectedMovement) =>
        val actualMovement = mobilitySimulator invokePrivate handleArrivingEv(
          ev,
          prices,
          availablePoints,
          maxDistance
        )

        actualMovement shouldBe expectedMovement
      }
    }

    "get time until next event" in {
      val mobilitySimulator = mobSim()

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
        val actualNextEvent =
          mobilitySimulator invokePrivate getTimeUntilNextEvent(
            evs,
            givenSimulationStart
          )

        actualNextEvent shouldBe expectedNextEvent
      }
    }

    "update electricVehicles correctly" in {
      val mobilitySimulator = mobSim()

      val updateElectricVehicles =
        PrivateMethod[Unit](Symbol("updateElectricVehicles"))

      val cases = Table(
        "updatedMovements",
        Seq(
          Movement(
            cs0.uuid,
            ev1.copy(
              homePoi = charging_hub_highwayPoi,
              chargingAtHomePossible = false
            )
          )
        ),
        Seq(
          Movement(cs0.uuid, ev1.copy(workPoi = givenHomePoi)),
          Movement(cs0.uuid, ev2.copy(chosenChargingStation = Some(cs2.uuid)))
        ),
        Seq(
          Movement(cs0.uuid, ev1.copy(storedEnergy = zero)),
          Movement(cs0.uuid, ev2.setChargingAtSimona()),
          Movement(
            cs0.uuid,
            ev3.copy(finalDestinationPoi = Some(charging_hub_townPoi))
          )
        )
      )

      forAll(cases) { updatedMovements =>
        mobilitySimulator invokePrivate updateElectricVehicles(updatedMovements)

        mobilitySimulator.electricVehicles.foreach { electricVehicle =>
          updatedMovements.foreach { movement =>
            if (movement.ev.uuid.equals(electricVehicle.uuid)) {
              electricVehicle shouldBe movement.ev
            }
          }
        }
      }
    }
  }
}
