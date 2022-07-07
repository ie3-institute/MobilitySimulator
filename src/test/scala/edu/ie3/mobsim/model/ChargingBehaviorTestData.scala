package edu.ie3.mobsim.model

import java.util.UUID

trait ChargingBehaviorTestData extends TripSimulationData {
  protected val currentlyAvailableChargingPoints: Map[UUID, Integer] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> Integer.valueOf(
        chargingStations.getChargingPoints
      )
    }.toMap
  }
}
