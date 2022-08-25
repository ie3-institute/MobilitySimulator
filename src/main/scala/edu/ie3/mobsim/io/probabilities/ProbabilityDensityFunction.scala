/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.MobilitySimulator
import edu.ie3.mobsim.exceptions.StochasticsException

import scala.collection.immutable.TreeMap
import scala.util.Random

final case class ProbabilityDensityFunction[T] private (
    pdf: Map[T, Double],
    cdf: TreeMap[Double, T],
    private val seed: Random
) {
  def sample(): T = {
    val p = seed.nextDouble()
    cdf.find { case (weight, _) =>
      weight >= p
    } match {
      case Some((_, value)) => value
      case None =>
        throw StochasticsException(
          "Sampling from empiric distribution failed to due unknown error."
        )
    }
  }
}

object ProbabilityDensityFunction {
  def apply[T](pdf: Map[T, Double]): ProbabilityDensityFunction[T] =
    apply(pdf, MobilitySimulator.seed)

  def apply[T](
      pdf: Map[T, Double],
      seed: Random
  ): ProbabilityDensityFunction[T] = {
    if (pdf.isEmpty) {
      throw StochasticsException(
        "Cannot build an empiric distribution function from an empty distribution."
      )
    } else {
      /* Build a cumulative density function with cumulated weight as key and value as double */
      val cdf: TreeMap[Double, T] = {
        val weightSum: Double = pdf.values.sum
        pdf
          .foldLeft(0d, TreeMap.empty[Double, T]) {
            case ((previousWeightSum, tempCdf), (value, weight)) =>
              val relativeWeight = weight / weightSum
              val currentWeightSum = previousWeightSum + relativeWeight
              (currentWeightSum, tempCdf + (currentWeightSum -> value))
          }
          ._2
      }

      new ProbabilityDensityFunction(pdf, cdf, seed)
    }
  }
}
