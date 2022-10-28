/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.config

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input.{Grid, Mobility}
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Simulation.Location
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.{Input, Simulation}
import edu.ie3.mobsim.exceptions.IllegalConfigException

object ConfigFailFast {
  def check(mobSimConfig: MobSimConfig): Unit = {
    check(mobSimConfig.mobsim.input)
    check(mobSimConfig.mobsim.simulation)
  }

  private def check(config: MobSimConfig.Mobsim.Input): Unit = config match {
    case Input(Grid(_, gridSource), Mobility(mobilitySource)) =>
      check(gridSource)
      check(mobilitySource)
  }

  private def check(csvParams: CsvParams): Unit = {
    val permissibleSeparators = Seq(",", ";", "\t")
    if (!permissibleSeparators.contains(csvParams.colSep))
      throw IllegalConfigException(
        s"Received illegal column separator '${csvParams.colSep}'. It has to be one of '${permissibleSeparators
            .mkString(", ")}'."
      )
  }

  private def check(config: MobSimConfig.Mobsim.Simulation): Unit =
    config match {
      case Simulation(location, _, numberOfEv, _, _, targetHomeChargingShare) =>
        check(location)
        if (numberOfEv < 0)
          throw IllegalConfigException(
            "The target number of electric vehicle has to be positive or zero!"
          )
        if (targetHomeChargingShare < 0 || targetHomeChargingShare > 1)
          throw IllegalConfigException(
            "The target home charging share has to be between 0.0 and 1.0."
          )
    }

  private def check(config: MobSimConfig.Mobsim.Simulation.Location): Unit =
    config match {
      case Location(
            chargingHubThresholdDistance,
            maxDistanceToChargingStation,
            maxDistanceToHomeChargingStation
          ) =>
        if (chargingHubThresholdDistance < 0.0)
          throw IllegalConfigException(
            "The charging hub threshold distance cannot be negative!"
          )

        if (maxDistanceToChargingStation < 0.0)
          throw IllegalConfigException(
            "The maximum permissible distance between a POI and a charging station cannot be negative!"
          )

        if (maxDistanceToHomeChargingStation < 0.0)
          throw IllegalConfigException(
            "The maximum permissible distance between a home POI and a charging station cannot be negative!"
          )
    }
}
