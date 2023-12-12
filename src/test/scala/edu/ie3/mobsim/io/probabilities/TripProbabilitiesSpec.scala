/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input.{Grid, Mobility}
import edu.ie3.mobsim.utils.PathsAndSources
import edu.ie3.test.common.UnitSpec

import java.nio.file.Paths

class TripProbabilitiesSpec extends UnitSpec {
  protected val averageCarUsage = 1.0
  "reads trip probabilities from files correctly" in {

    val gridConf = Grid("grid", CsvParams("/some/Path", ","))
    val mobSimPath =
      Paths.get("input", "mobilitySimulator")

    val mobilityConf = Mobility(CsvParams("", mobSimPath.toString))
    val pathsAndSources =
      PathsAndSources("testSim", Input(None, gridConf, mobilityConf), None)
    val tripProbabilities =
      TripProbabilities.read(pathsAndSources, ",", averageCarUsage, round15 = true)

    tripProbabilities.categoricalLocation.probabilitiesWeekday.size shouldBe 42
    tripProbabilities.categoricalLocation.probabilitiesSaturday.size shouldBe 42
    tripProbabilities.categoricalLocation.probabilitiesSunday.size shouldBe 42

    tripProbabilities.drivingSpeed.parametersWeekday.size shouldBe 12
    tripProbabilities.drivingSpeed.parametersSaturday.size shouldBe 12
    tripProbabilities.drivingSpeed.parametersSunday.size shouldBe 12

    tripProbabilities.firstDepartureOfDay.probabilityWeekday.pdf.size shouldBe 96
    tripProbabilities.firstDepartureOfDay.probabilitySaturday.pdf.size shouldBe 96
    tripProbabilities.firstDepartureOfDay.probabilitySunday.pdf.size shouldBe 96

    tripProbabilities.lastTripOfDay.probabilityWeekday.size shouldBe 96
    tripProbabilities.lastTripOfDay.probabilitySaturday.size shouldBe 96
    tripProbabilities.lastTripOfDay.probabilitySunday.size shouldBe 96

    tripProbabilities.parkingTime.probabilitiesWeekday.size shouldBe 672
    tripProbabilities.parkingTime.probabilitiesSaturday.size shouldBe 672
    tripProbabilities.parkingTime.probabilitiesSunday.size shouldBe 672
    tripProbabilities.parkingTime.round15 shouldBe true

    tripProbabilities.poiTransition.probabilitiesWeekday.size shouldBe 672
    tripProbabilities.poiTransition.probabilitiesSaturday.size shouldBe 672
    tripProbabilities.poiTransition.probabilitiesSunday.size shouldBe 672

    tripProbabilities.tripDistance.probabilitiesWeekday.size shouldBe 4704
    tripProbabilities.tripDistance.probabilitiesSaturday.size shouldBe 4704
    tripProbabilities.tripDistance.probabilitiesSunday.size shouldBe 4704
  }

}
