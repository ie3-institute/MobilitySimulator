/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import akka.actor.ActorRef
import edu.ie3.mobsim.io.geodata.PoiEnums.PoiTypeDictionary.WORK
import edu.ie3.mobsim.model.{ChargingBehaviorTestData, ElectricVehicle}
import edu.ie3.simona.api.data.ev.ExtEvData

import scala.collection.immutable.SortedSet

trait MobilitySimulatorTestData extends ChargingBehaviorTestData {
  val evData: ExtEvData = new ExtEvData(ActorRef.noSender, ActorRef.noSender)

  def electricVehicles(seq: Seq[ElectricVehicle]): SortedSet[ElectricVehicle] =
    SortedSet.empty[ElectricVehicle] ++ seq.toSet

  def evIsParking(ev: ElectricVehicle): ElectricVehicle = {
    ev.copyWith(
      storedEnergy = half,
      destinationPoiType = WORK,
      destinationCategoricalLocation = workPoi.categoricalLocation,
      destinationPoi = workPoi,
      parkingTimeStart = givenSimulationStart,
      departureTime = givenSimulationStart.plusHours(5)
    )
  }

  def evIsDeparting(ev: ElectricVehicle): ElectricVehicle = {
    ev.copyWith(
      storedEnergy = half,
      destinationPoiType = WORK,
      destinationCategoricalLocation = workPoi.categoricalLocation,
      destinationPoi = workPoi,
      parkingTimeStart = givenSimulationStart.plusHours(-4),
      departureTime = givenSimulationStart
    )
  }

  protected val ev1parking: ElectricVehicle = evIsParking(ev1)
  protected val ev1departing: ElectricVehicle = evIsDeparting(ev1)
  protected val ev2parking: ElectricVehicle = evIsParking(ev2)
  protected val ev2departing: ElectricVehicle = evIsDeparting(ev2)

  val mobSim: MobilitySimulator = new MobilitySimulator(
    evData = evData,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = electricVehicles(Seq(ev1, ev2, ev3, ev4, ev5)),
    chargingHubTownIsPresent = true,
    chargingHubHighwayIsPresent = true,
    ioUtils = ioUtils,
    categoricalLocation = categoricalLocation,
    drivingSpeed = drivingSpeed,
    firstDepartureOfDay = firstDepartureOfDay,
    lastTripOfDay = lastTripOfDay,
    parkingTime = parkingTime,
    poiTransition = poiTransition,
    tripDistance = tripDistance,
    maxDistanceFromPoi = maxDistance,
    thresholdChargingHubDistance = maxDistance
  )

}
