/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import akka.actor.ActorRef
import edu.ie3.mobsim.model.{ChargingBehaviorTestData, ElectricVehicle}
import edu.ie3.simona.api.data.ev.ExtEvData

import scala.collection.immutable.SortedSet

trait MobilitySimulatorTestData extends ChargingBehaviorTestData {
  val evData: ExtEvData = new ExtEvData(ActorRef.noSender, ActorRef.noSender)
  var electricVehicles: SortedSet[ElectricVehicle] =
    SortedSet.empty[ElectricVehicle] ++ Seq(ev1, ev2, ev3, ev4, ev5).toSet



  val mobSim: MobilitySimulator = new MobilitySimulator(
    evData = evData,
    chargingStations = chargingStations,
    poisWithSizes = poisWithSizes,
    startTime = givenSimulationStart,
    electricVehicles = electricVehicles,
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
