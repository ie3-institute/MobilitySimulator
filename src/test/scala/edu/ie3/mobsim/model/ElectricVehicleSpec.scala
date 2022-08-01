/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.probabilities.ProbabilityDensityFunction
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units

import java.time.ZonedDateTime
import javax.measure.quantity.Energy
import scala.collection.{immutable, mutable}

class ElectricVehicleSpec extends UnitSpec with TripSimulationTestData {
  "Building and assigning evs" when {

    "building the car models" should {
      "assign the correct properties" in {
        ElectricVehicle.buildEv(
          "test_car",
          givenModel,
          givenHomePoi,
          givenWorkPoi,
          givenSimulationStart,
          givenFirstDeparture,
          isChargingAtHomePossible = true
        ) match {
          case ElectricVehicle(
                simulationStart,
                uuid,
                id,
                model,
                batteryCapacity,
                acChargingPower,
                dcChargingPower,
                consumption,
                homePoi,
                workPoi,
                storedEnergy,
                destinationPoi,
                parkingTimeStart,
                departureTime,
                chargingAtHomePossible,
                chosenChargingStation,
                chargingAtSimona,
                finalDestinationPoi,
                remainingDistanceAfterChargingHub,
                chargingPricesMemory
              ) =>
            simulationStart shouldBe givenSimulationStart
            id shouldBe "test_car"
            model shouldBe "cool_producer cool_model"
            batteryCapacity shouldBe givenModel.capacity
            acChargingPower shouldBe givenModel.acPower
            dcChargingPower shouldBe givenModel.dcPower
            consumption shouldBe givenModel.consumption
            homePoi shouldBe givenHomePoi
            workPoi shouldBe givenWorkPoi
            storedEnergy shouldBe givenModel.capacity
            chargingAtSimona shouldBe false
            destinationPoi shouldBe givenHomePoi
            parkingTimeStart shouldBe simulationStart
            departureTime shouldBe givenFirstDeparture
            chosenChargingStation shouldBe None
            chargingAtHomePossible shouldBe true
            finalDestinationPoi shouldBe None
            remainingDistanceAfterChargingHub shouldBe None
            chargingPricesMemory shouldBe immutable.Queue[Double]()
        }
      }

      "adapt the dc charging power" in {
        ElectricVehicle.buildEv(
          "test_car",
          givenModel.copy(
            dcPower = Quantities.getQuantity(0d, PowerSystemUnits.KILOWATT)
          ),
          givenHomePoi,
          givenWorkPoi,
          givenSimulationStart,
          givenFirstDeparture,
          isChargingAtHomePossible = true
        ) match {
          case model: ElectricVehicle =>
            model.getSRatedDC shouldBe givenModel.acPower
        }
      }

      "adapt the first departure" in {
        ElectricVehicle.buildEv(
          "test_car",
          givenModel,
          givenHomePoi,
          givenWorkPoi,
          givenSimulationStart,
          givenSimulationStart,
          isChargingAtHomePossible = true
        ) match {
          case model: ElectricVehicle =>
            model.departureTime shouldBe givenSimulationStart.plusMinutes(1L)
        }
      }
    }

    /* Building with random attributes is not tested, because it is assumed, that the random processes are tested
     * separately. Other than that, there is no additional logic in the method. */

    "assigning the first cars" should {
      val assignInitialHomeChargingCars =
        PrivateMethod[Iterable[ElectricVehicle]](
          Symbol("assignInitialHomeChargingCars")
        )

      "provide the correct amount of home charging cars" in {
        forAll(
          Table(
            ("amountOfHomeChargingCars", "expectedAmount"),
            (0, 0),
            (10, 10),
            (100, 100),
            (120, 100)
          )
        ) { case (amountOfHomeChargingCars: Int, expectedAmount: Int) =>
          val initialCars =
            ElectricVehicle invokePrivate assignInitialHomeChargingCars(
              amountOfHomeChargingCars,
              ProbabilityDensityFunction(
                Range(0, 100)
                  .map(cnt => givenHomePoi.copy(id = s"home_poi_$cnt") -> 1.0)
                  .toMap
              ),
              givenWorkPoiPdf,
              givenModelPdf,
              givenFirstDepartureMetaData,
              givenSimulationStart
            )
          /* There are 100 home charging stations available. The actual amount of cars is limited by the minimum of
           * their target amount and the amount of available home POI */
          initialCars.size shouldBe expectedAmount
        }
      }
    }

    "assigning the remaining cars" should {
      val testCases = Table(
        (
          "amountOfEvsInArea",
          "amountOfHomeChargingCars",
          "amountOfAssignedCars",
          "expectedHomeChargingAmount",
          "expectedOverallAmount"
        ),
        (100, 0, 0, 0, 100), // All cars already assigned
        (100, 10, 10, 0, 90),
        (100, 100, 100, 0, 0),
        (100, 0, 10, 0, 90), // Too many already assigned
        (100, 10, 20, 0, 80),
        (100, 100, 110, 0, 0),
        (100, 30, 10, 20, 90)
      )

      "determine the amount of needed cars correctly" in {
        val determineUnassignedCars =
          PrivateMethod[(Int, Int)](Symbol("determineUnassignedCars"))

        forAll(testCases) {
          case (
                amountOfEvsInArea: Int,
                amountOfHomeChargingCars: Int,
                amountOfAssignedCars: Int,
                expectedHomeChargingAmount: Int,
                expectedOverallAmount: Int
              ) =>
            ElectricVehicle invokePrivate determineUnassignedCars(
              amountOfEvsInArea,
              amountOfHomeChargingCars,
              amountOfAssignedCars
            ) match {
              case (amountOfHomeCharging, overallAmount) =>
                amountOfHomeCharging shouldBe expectedHomeChargingAmount
                overallAmount shouldBe expectedOverallAmount
            }
        }
      }

      "provide the correct portion of cars with and without home charging option" in {
        val assignRemainingCars = PrivateMethod[Iterable[ElectricVehicle]](
          Symbol("assignRemainingCars")
        )
        forAll(testCases) {
          case (
                amountOfEvsInArea: Int,
                amountOfHomeChargingCars: Int,
                amountOfAssignedCars: Int,
                expectedHomeChargingAmount: Int,
                expectedOverallAmount: Int
              ) =>
            val additionalCars =
              ElectricVehicle invokePrivate assignRemainingCars(
                amountOfEvsInArea,
                amountOfHomeChargingCars,
                amountOfAssignedCars,
                givenHomePoiPdf,
                givenHomePoiPdf,
                givenWorkPoiPdf,
                givenModelPdf,
                givenFirstDepartureMetaData,
                givenSimulationStart
              )

            additionalCars should have size expectedOverallAmount
            additionalCars.count(
              _.chargingAtHomePossible
            ) shouldBe expectedHomeChargingAmount
        }
      }
    }

    "creating the cars" should {
      "provide the correct amount and portion" in {
        /* Create 200 home POI, where 100 are available for home charging */
        val homePoisWithSizes = (Range(0, 100).map { cnt =>
          givenHomePoi.copy(
            id = s"home_poi_$cnt",
            nearestChargingStations = Map(
              givenChargingStation -> Quantities.getQuantity(1.0, Units.METRE)
            )
          )
        } ++ Range(100, 200).map { cnt =>
          givenHomePoi.copy(
            id = s"home_poi_$cnt"
          )
        }).map(_ -> 1.0).toMap

        forAll(
          Table(
            ("targetAmount", "targetShare", "expectedAmountOfHomeCharging"),
            (10, 1.0, 10),
            (100, 1.0, 100),
            (100, 0.1, 10),
            (100, 0.125, 13)
          )
        ) {
          case (
                targetAmount: Int,
                targetShare: Double,
                expectedAmountOfHomeCharging: Int
              ) =>
            val evs = ElectricVehicle.createEvs(
              targetAmount,
              homePoisWithSizes,
              givenWorkPoiPdf,
              Set(givenChargingStation),
              givenSimulationStart,
              targetShare,
              givenModelPdf,
              givenFirstDepartureMetaData
            )

            evs should have size targetAmount
            evs.count(
              _.chargingAtHomePossible
            ) shouldBe expectedAmountOfHomeCharging
        }
      }
    }

    "An electricVehicle" should {
      "check if home charging is possible" in {
        evWithHomeCharging.chargingAtHomePossible shouldBe true
        evWithoutHomeCharging.chargingAtHomePossible shouldBe false
      }

      "copy object with new stored energy" in {
        val evFull: ElectricVehicle =
          evWithHomeCharging.copyWith(evWithHomeCharging.getStoredEnergy)
        evFull.getStoredEnergy shouldBe givenModel.capacity

        val zero: ComparableQuantity[Energy] =
          Quantities.getQuantity(0, PowerSystemUnits.KILOWATTHOUR)
        val evEmpty: ElectricVehicle = evWithHomeCharging.copyWith(zero)
        evEmpty.getStoredEnergy shouldBe zero
      }

      "copy object with updated charging at simona information" in {
        val evChargingAtSimona: ElectricVehicle =
          evWithHomeCharging.setChargingAtSimona()
        evChargingAtSimona.chargingAtSimona shouldBe true

        val evNotChargingAtSimona: ElectricVehicle =
          evWithHomeCharging.removeChargingAtSimona()
        evNotChargingAtSimona.chargingAtSimona shouldBe false
      }

      "copy object with chosen charging station" in {
        val evSetChargingStation: ElectricVehicle =
          evWithHomeCharging.setChosenChargingStation(Some(cs6.uuid))
        evSetChargingStation.chosenChargingStation shouldBe Some(cs6.uuid)

        val evNoChargingStation: ElectricVehicle =
          evWithHomeCharging.setChosenChargingStation(None)
        evNoChargingStation.chosenChargingStation shouldBe None
      }

      "return the correct poiType for the destinationPoi" in {
        val ev: ElectricVehicle = ev1.copyWith(
          storedEnergy = ev1.storedEnergy,
          bbpgPoi,
          parkingTimeStart = ev1.parkingTimeStart,
          departureTime = ev1.departureTime
        )

        ev.getDestinationPoiType shouldBe bbpgPoi.getPoiType
      }

      "return the correct departure tick" in {
        val time: ZonedDateTime = ZonedDateTime.now().plusHours(1)

        val ev: ElectricVehicle = ev1.copyWith(
          storedEnergy = ev1.storedEnergy,
          destinationPoi = ev1.destinationPoi,
          parkingTimeStart = ev1.parkingTimeStart,
          time
        )

        ev.departureTime shouldBe time
      }

      "update charging price memory correctly" in {
        val evNoQueue: ElectricVehicle = ev1
        val queue: mutable.Queue[Double] = mutable.Queue.empty

        for (n <- 0 to 10) {
          queue.enqueue(n.doubleValue())
        }

        var updatedEv: ElectricVehicle =
          evNoQueue.updateChargingPricesMemory(queue)
        updatedEv.chargingPricesMemory shouldBe queue

        for (n <- 11 to 20) {
          queue.enqueue(n.doubleValue())
        }

        updatedEv = updatedEv.updateChargingPricesMemory(queue)
        updatedEv.chargingPricesMemory shouldBe queue.drop(2)

      }
    }
  }
}
