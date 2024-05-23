/*
 * © 2024. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils.sq

import squants.energy.KilowattHours
import squants.space.Kilometers
import squants.{Energy, Length}

object SquantsUtils {
  implicit class RichDistance(distance: Length) {
    def calcEnergy(that: SpecificEnergyDistance): Energy = KilowattHours(
      distance.toKilometers * that.toKilowattHoursPerKilometer
    )
  }

  implicit class RichEnergy(energy: Energy) {
    def calcDistance(that: SpecificEnergyDistance): Length = Kilometers(
      energy.toKilowattHours / that.toKilowattHoursPerKilometer
    )
  }
}
