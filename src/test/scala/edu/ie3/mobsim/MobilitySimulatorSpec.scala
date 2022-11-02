/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.mobsim.model.EvMovement
import edu.ie3.mobsim.model.ElectricVehicle
import edu.ie3.test.common.UnitSpec

import java.util.UUID

class MobilitySimulatorSpec extends UnitSpec with MobilitySimulatorTestData {
  "MobilitySimulator" should {

    "define movement correctly" in {
      val mobilitySimulator = mobSim()

      val defineMovements =
        PrivateMethod[(Seq[ElectricVehicle], Seq[ElectricVehicle])](
          Symbol("defineMovements")
        )

      val cases = Table(
        ("parkingEvs", "departingEvs"),
        (
          setEvsAsParking(Seq(ev1, ev2, ev3)),
          Seq.empty[ElectricVehicle]
        ),
        (
          setEvsAsParking(Seq(ev1, ev2)),
          setEvsAsDeparting(Seq(ev3))
        ),
        (
          setEvsAsParking(Seq(ev1)),
          setEvsAsDeparting(Seq(ev2, ev3))
        ),
        (
          Seq.empty[ElectricVehicle],
          setEvsAsDeparting(Seq(ev1, ev2, ev3))
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
        PrivateMethod[(Seq[EvMovement], Map[UUID, Int])](
          Symbol("handleDepartures")
        )

      val cases = Table(
        ("departingEvs", "expectedChargingPoints", "expectedMovements"),
        (Seq(ev1), 1, Seq(EvMovement(cs6.uuid, ev1))),
        (
          Seq(ev1, ev2),
          2,
          Seq(EvMovement(cs6.uuid, ev1), EvMovement(cs6.uuid, ev2))
        ),
        (
          Seq(ev1, ev2, ev3),
          3,
          Seq(
            EvMovement(cs6.uuid, ev1),
            EvMovement(cs6.uuid, ev2),
            EvMovement(cs6.uuid, ev3)
          )
        )
      )

      forAll(cases) { (departingEvs, expectedCsCount, expectedMovements) =>
        val (movements, actualChargingPoints) =
          mobilitySimulator invokePrivate handleDepartures(
            setEvsAsDeparting(departingEvs),
            chargingPointsAllTaken
          )

        actualChargingPoints shouldBe Map(
          cs6.uuid -> expectedCsCount
        )
        movements.map(_.cs) shouldBe expectedMovements.map(_.cs)
        movements.map(
          _.ev.uuid
        ) should contain theSameElementsAs expectedMovements.map(_.ev.uuid)
      }
    }

    "handle departing evs" in {
      val mobilitySimulator = mobSim()

      val handleDepartingEvs =
        PrivateMethod[(Map[UUID, Int], Seq[EvMovement])](
          Symbol("handleDepartingEvs")
        )

      val cases = Table(
        ("evs", "expectedFreeCs"),
        (Seq(ev1), Map(cs6.uuid -> 1)),
        (Seq(ev1, ev2), Map(cs6.uuid -> 2)),
        (Seq(ev1, ev2, ev3), Map(cs6.uuid -> 3))
      )

      forAll(cases) { (evs, expectedFreeCs) =>
        val (actualFreeCs, actualMovements) =
          mobilitySimulator invokePrivate handleDepartingEvs(
            setEvsAsDeparting(evs)
          )

        val expectedMovements = evs.toSeq.map { ev =>
          EvMovement(
            cs6.uuid,
            ev.copy(
              storedEnergy = half,
              destinationPoi = workPoi,
              destinationPoiType = PoiTypeDictionary.WORK,
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

      val handleDepartingEv = PrivateMethod[Option[EvMovement]](
        Symbol("handleDepartingEv")
      )

      val cases = Table(
        ("ev", "expectedResults"),
        (
          evChargingAtSimonaWithStation,
          Some(
            EvMovement(
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
          case Some(EvMovement(cs, ev)) =>
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

      val handleParkingEvs = PrivateMethod[Seq[EvMovement]](
        Symbol("handleParkingEvs")
      )

      val cases = Table(
        "evs",
        setEvsAsParking(Seq(ev1)),
        setEvsAsParking(Seq(ev1, ev2)),
        setEvsAsParking(Seq(ev1, ev2, ev3))
      )

      forAll(cases) { evs =>
        val actualMovements = mobilitySimulator invokePrivate handleParkingEvs(
          evs,
          pricesAtChargingStation,
          chargingPointsAllFree,
          maxDistance
        )

        val expectedMovements: Seq[EvMovement] = evs.toSeq.map { ev =>
          EvMovement(
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

      val handleArrivingEv = PrivateMethod[Option[EvMovement]](
        Symbol("handleArrivingEv")
      )

      val cases = Table(
        ("ev", "prices", "availablePoints", "expectedMovement"),
        (
          arrivingEv,
          Map(cs6.uuid -> 0.0),
          Map(cs6.uuid -> 1),
          Some(
            EvMovement(
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
        (Seq(arrivingEv), 3600),
        (Seq(ev2), 2700),
        (Seq(arrivingEv, ev2), 2700)
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
          EvMovement(
            cs0.uuid,
            ev1.copy(
              homePoi = charging_hub_highwayPoi,
              chargingAtHomePossible = false
            )
          )
        ),
        Seq(
          EvMovement(cs0.uuid, ev1.copy(workPoi = givenHomePoi)),
          EvMovement(cs0.uuid, ev2.copy(chosenChargingStation = Some(cs2.uuid)))
        ),
        Seq(
          EvMovement(cs0.uuid, ev1.copy(storedEnergy = zero)),
          EvMovement(cs0.uuid, ev2.setChargingAtSimona()),
          EvMovement(
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
