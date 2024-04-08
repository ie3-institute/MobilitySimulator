/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import squants.energy.KilowattHours
import squants.space.Kilometers
import squants.{Energy, Length}

object DefaultQuantities {

  val ZERO_DISTANCE: Length = Kilometers(0)
  val ZERO_ENERGY: Energy = KilowattHours(0)

  val SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD: Double = 0.2
  val SOC_OF_10_PERCENT: Double = 0.1
  val SOC_OF_30_PERCENT: Double = 0.3
  val SOC_OF_70_PERCENT: Double = 0.7

  val REMAINING_DISTANCE_AFTER_MODIFIED_CHARGING_HUB_STOP: Length =
    Kilometers(10)

}
