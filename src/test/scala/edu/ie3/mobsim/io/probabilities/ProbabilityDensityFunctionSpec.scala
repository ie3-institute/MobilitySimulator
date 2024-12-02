/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.exceptions.StochasticsException
import edu.ie3.test.common.UnitSpec

import scala.collection.immutable.TreeMap

class ProbabilityDensityFunctionSpec extends UnitSpec {
  private val randomSeed = new scala.util.Random(42)

  "Modeling empiric probability density functions" when {
    "trying to build" should {
      "fail when given an empty distribution" in {
        val exception = intercept[StochasticsException] {
          ProbabilityDensityFunction(Map.empty[Int, Double], randomSeed)
        }

        exception.msg shouldBe "Cannot build an empiric distribution function from an empty distribution."
      }

      "produce the correct cdf" in {
        val givenPdf = Map(
          1 -> 5.0,
          2 -> 20.0,
          3 -> 50.0,
          4 -> 10.0,
          5 -> 15.0,
        ) // Can be seen as percent
        val expectedCdf =
          TreeMap(0.15 -> 5, 0.2 -> 1, 0.4 -> 2, 0.9 -> 3, 1.0 -> 4)

        ProbabilityDensityFunction(givenPdf, randomSeed) match {
          case ProbabilityDensityFunction(pdf, cdf, seed) =>
            pdf shouldBe givenPdf
            expectedCdf.zip(cdf).foreach {
              case (
                    (expectedWeight, expectedValue),
                    (actualWeight, actualValue),
                  ) =>
                actualWeight shouldBe expectedWeight +- 1e-6
                actualValue shouldBe expectedValue
            }
            seed shouldBe randomSeed
        }
      }

      "produce the correct cdf given 0 probabilities" in {
        val givenPdf = Map(
          1 -> 5.0,
          2 -> 0d,
          3 -> 70.0,
          4 -> 0d,
          5 -> 25.0,
        ) // Can be seen as percent
        val expectedCdf =
          TreeMap(0.25 -> 5, 0.3 -> 1, 1.0 -> 3)

        ProbabilityDensityFunction(givenPdf, randomSeed) match {
          case ProbabilityDensityFunction(pdf, cdf, seed) =>
            pdf shouldBe givenPdf
            expectedCdf.zip(cdf).foreach {
              case (
                    (expectedWeight, expectedValue),
                    (actualWeight, actualValue),
                  ) =>
                actualWeight shouldBe expectedWeight +- 1e-6
                actualValue shouldBe expectedValue
            }
            seed shouldBe randomSeed
        }
      }
    }

    "sampling" should {
      "nearly provide the same empirical distribution" in {
        val pdf = ProbabilityDensityFunction(
          Map(1 -> 0.05, 2 -> 0.2, 3 -> 0.5, 4 -> 0.1, 5 -> 0.15),
          randomSeed,
        )

        val noOfSamples = 10000
        val samplesToPortion = (0 to noOfSamples)
          .map(_ => pdf.sample())
          .groupBy(sample => sample)
          .map { case (k, v) =>
            (k, v.length.doubleValue / noOfSamples)
          }
          .toSeq
          .sortBy(_._1)
          .toMap

        samplesToPortion.keys should contain theSameElementsAs pdf.pdf.keys
        samplesToPortion.zip(pdf.pdf).foreach {
          case ((sample, portion), (value, weight)) =>
            sample shouldBe value
            portion shouldBe weight +- 0.01 // Shall meet the correct portion / foreseen weight by 1%-tolerance
        }
      }
    }
  }
}
