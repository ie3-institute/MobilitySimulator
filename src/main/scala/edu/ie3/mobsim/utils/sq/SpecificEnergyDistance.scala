/*
 * © 2023. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils.sq

import squants._
import squants.energy.KilowattHours
import squants.space.Kilometers

import scala.util.Try

/** Represents a specific energy consumption per kilometer.
  *
  * In kWh / km
  *
  * Based on [[squants.thermal.ThermalCapacity]] by garyKeorkunian
  */
final class SpecificEnergyDistance private (
    val value: Double,
    val unit: SpecificEnergyDistanceUnit,
) extends Quantity[SpecificEnergyDistance] {

  def dimension: SpecificEnergyDistance.type = SpecificEnergyDistance

  def *(that: Length): Energy = KilowattHours(
    this.toKilowattHoursPerKilometer * that.toKilometers
  )

  def /(that: Energy): Length = Kilometers(
    that.toKilowattHours / this.toKilowattHoursPerKilometer
  )

  def toKilowattHoursPerKilometer: Double = to(
    KilowattHoursPerKilometer
  )
}

object SpecificEnergyDistance extends Dimension[SpecificEnergyDistance] {
  def apply[A](n: A, unit: SpecificEnergyDistanceUnit)(implicit
      num: Numeric[A]
  ) =
    new SpecificEnergyDistance(num.toDouble(n), unit)
  def apply(value: Any): Try[SpecificEnergyDistance] = parse(value)
  def name = "SpecificEnergyDistance"
  def primaryUnit: KilowattHoursPerKilometer.type =
    KilowattHoursPerKilometer
  def siUnit: KilowattHoursPerKilometer.type =
    KilowattHoursPerKilometer
  def units: Set[UnitOfMeasure[SpecificEnergyDistance]] = Set(
    KilowattHoursPerKilometer
  )
}

trait SpecificEnergyDistanceUnit
    extends UnitOfMeasure[SpecificEnergyDistance]
    with UnitConverter {
  def apply[A](n: A)(implicit num: Numeric[A]): SpecificEnergyDistance =
    SpecificEnergyDistance(n, this)
}

object KilowattHoursPerKilometer
    extends SpecificEnergyDistanceUnit
    with PrimaryUnit
    with SiUnit {
  def symbol = "kWh/km"
}
