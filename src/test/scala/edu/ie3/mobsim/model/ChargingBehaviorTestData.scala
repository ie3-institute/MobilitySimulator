/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

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

  protected val availableChargingPoints: Map[UUID, Int] = {
    chargingStations.map { chargingStations =>
      chargingStations.getUuid -> chargingStations.getChargingPoints
    }.toMap
  }
}
