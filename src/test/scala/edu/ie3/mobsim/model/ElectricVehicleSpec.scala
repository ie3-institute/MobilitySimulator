/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary
import edu.ie3.test.common.UnitSpec
import edu.ie3.util.quantities.PowerSystemUnits
import squants.energy.Kilowatts
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities

import java.time.ZonedDateTime
import scala.collection.immutable.Queue

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
                _,
                id,
                evType,
                homePoi,
                workPoi,
                storedEnergy,
                destinationPoi,
                destinationPoiType,
                parkingTimeStart,
                departureTime,
                chargingAtHomePossible,
                chosenChargingStation,
                chargingAtSimona,
                finalDestinationPoi,
                finalDestinationPoiType,
                remainingDistanceAfterChargingHub,
                chargingPricesMemory
              ) =>
            simulationStart shouldBe givenSimulationStart
            id shouldBe "test_car"
            evType shouldBe givenModel
            homePoi shouldBe givenHomePoi
            workPoi shouldBe givenWorkPoi
            storedEnergy shouldBe Quantities.getQuantity(
              givenModel.capacity.toKilowattHours,
              PowerSystemUnits.KILOWATTHOUR
            )
            chargingAtSimona shouldBe false
            destinationPoi shouldBe givenHomePoi
            destinationPoiType shouldBe PoiTypeDictionary.HOME
            parkingTimeStart shouldBe simulationStart
            departureTime shouldBe givenFirstDeparture
            chosenChargingStation shouldBe None
            chargingAtHomePossible shouldBe true
            finalDestinationPoi shouldBe None
            finalDestinationPoiType shouldBe None
            remainingDistanceAfterChargingHub shouldBe None
            chargingPricesMemory shouldBe Queue[Double]()
        }
      }

      "adapt the dc charging power" in {
        ElectricVehicle.buildEv(
          "test_car",
          givenModel.copy(
            dcPower = Kilowatts(0d)
          ),
          givenHomePoi,
          givenWorkPoi,
          givenSimulationStart,
          givenFirstDeparture,
          isChargingAtHomePossible = true
        ) match {
          case model: ElectricVehicle =>
            model.getPRatedDC shouldBe Quantities.getQuantity(
              givenModel.acPower.toKilowatts,
              PowerSystemUnits.KILOWATT
            )
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

    "An electricVehicle" should {
      "check if home charging is possible" in {
        evWithHomeCharging.chargingAtHomePossible shouldBe true
        evWithoutHomeCharging.chargingAtHomePossible shouldBe false
      }

      "copy object with new stored energy" in {
        val evFull: ElectricVehicle =
          evWithHomeCharging.copyWith(evWithHomeCharging.getStoredEnergy)
        evFull.getStoredEnergy shouldBe Quantities.getQuantity(
          givenModel.capacity.toKilowattHours,
          PowerSystemUnits.KILOWATTHOUR
        )

        val zero: ComparableQuantity[javax.measure.quantity.Energy] =
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
          PoiTypeDictionary.LEISURE,
          parkingTimeStart = ev1.parkingTimeStart,
          departureTime = ev1.departureTime
        )

        ev.destinationPoiType shouldBe PoiTypeDictionary.LEISURE
      }

      "return the correct departure tick" in {
        val time: ZonedDateTime = ZonedDateTime.now().plusHours(1)

        val ev: ElectricVehicle = ev1.copyWith(
          storedEnergy = ev1.storedEnergy,
          destinationPoi = ev1.destinationPoi,
          destinationPoiType = ev1.destinationPoiType,
          parkingTimeStart = ev1.parkingTimeStart,
          time
        )

        ev.departureTime shouldBe time
      }

      "update charging price memory correctly" in {
        val evNoQueue: ElectricVehicle = ev1
        val queue: Queue[Double] =
          (1 to 10).map { x => x.doubleValue() }.to(Queue)

        var updatedEv: ElectricVehicle =
          evNoQueue.updateChargingPricesMemory(queue)
        updatedEv.chargingPricesMemory shouldBe queue

        val secondQueue: Queue[Double] =
          (1 to 25).map { x => x.doubleValue() }.to(Queue)

        updatedEv = updatedEv.updateChargingPricesMemory(secondQueue)
        updatedEv.chargingPricesMemory shouldBe secondQueue.drop(5)
      }
    }
  }
}
