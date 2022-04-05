/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.mobsim.MobilitySimulator.seed
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units.METRE
import edu.ie3.util.quantities.PowerSystemUnits.KILOMETRE

import java.time.ZonedDateTime
import java.time.temporal.ChronoUnit
import javax.measure.quantity.{Angle, Length}

object utils {

  /** Convert time to corresponding simulation tick
    * @param simulationStart
    *   Wall clock time of the simulation start
    * @param time
    *   time to be converted to tick
    * @return
    *   tick
    */
  def toTick(simulationStart: ZonedDateTime, time: ZonedDateTime): Long = {
    simulationStart.until(time, ChronoUnit.SECONDS)
  }
}
