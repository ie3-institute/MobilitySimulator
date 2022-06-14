/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.util.quantities.PowerSystemUnits.{KILOMETRE, KILOWATTHOUR}
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities.getQuantity

import javax.measure.quantity.{Energy, Length}

object DefaultQuantities {

  val ZERO_DISTANCE: ComparableQuantity[Length] = getQuantity(0, KILOMETRE)
  val ZERO_ENERGY: ComparableQuantity[Energy] = getQuantity(0, KILOWATTHOUR)

  val SOC_OF_20_PERCENT_CHARGING_HUB_THRESHOLD: Double = 0.2
  val SOC_OF_10_PERCENT: Double = 0.1
  val SOC_OF_30_PERCENT: Double = 0.3
  val SOC_OF_70_PERCENT: Double = 0.7
  val REMAINING_DISTANCE_AFTER_MODIFIED_CHARGING_HUB_STOP
      : ComparableQuantity[Length] = getQuantity(10, KILOMETRE)

}
