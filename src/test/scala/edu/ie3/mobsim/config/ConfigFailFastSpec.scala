/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.config

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Simulation.Location
import edu.ie3.mobsim.exceptions.IllegalConfigException
import edu.ie3.test.common.UnitSpec

class ConfigFailFastSpec extends UnitSpec {

  private def createValidMobSimConfig(
      gridSource: CsvParams = CsvParams(path = "poi.csv", colSep = ","),
      mobilitySource: CsvParams =
        CsvParams(path = "departure.csv", colSep = ","),
      averageCarUsage: Double = 0.6,
      numberOfEv: Int = 10,
      targetHomeChargingShare: Double = 0.5,
  ): MobSimConfig = {
    MobSimConfig(
      mobsim = MobSimConfig.Mobsim(
        input = MobSimConfig.Mobsim.Input(
          evInputSource = None,
          grid = MobSimConfig.Mobsim.Input.Grid("gridName", gridSource),
          mobility = MobSimConfig.Mobsim.Input.Mobility(mobilitySource),
        ),
        output = MobSimConfig.Mobsim.Output(
          outputDir = Some("output"),
          writeMovements = true,
        ),
        simulation = MobSimConfig.Mobsim.Simulation(
          name = "testSimulation",
          startDate = "2025-01-01",
          averageCarUsage = averageCarUsage,
          location = MobSimConfig.Mobsim.Simulation.Location(
            maxDistanceToChargingStation = 500.0,
            maxDistanceToHomeChargingStation = 30.0,
            chargingHubThresholdDistance = 50.0,
          ),
          numberOfEv = numberOfEv,
          targetHomeChargingShare = targetHomeChargingShare,
          round15 = false,
        ),
      )
    )
  }

  "ConfigFailFast" should {

    "throw an exception when the column separator in CsvParams is not permissible" in {
      val invalidCsvParams = CsvParams(path = "departure.csv", colSep = "|")
      val invalidMobSimConfig =
        createValidMobSimConfig(mobilitySource = invalidCsvParams)

      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include("Received illegal column separator")
    }

    "not throw an exception when the column separator in CsvParams is permissible" in {
      val validCsvParams = CsvParams(path = "departure.csv", colSep = ",")
      val validMobSimConfig =
        createValidMobSimConfig(mobilitySource = validCsvParams)

      noException should be thrownBy ConfigFailFast.check(validMobSimConfig)
    }

    "throw an exception when average car usage is out of bounds" in {
      val invalidMobSimConfig = createValidMobSimConfig(averageCarUsage = 1.5)

      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The average car usage should be between 0 and 1"
      )
    }

    "throw an exception when the number of electric vehicles is negative" in {
      val invalidMobSimConfig = createValidMobSimConfig(numberOfEv = -1)

      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The target number of electric vehicle has to be positive or zero!"
      )
    }

    "throw an exception when the target home charging share is out of bounds" in {
      val invalidMobSimConfig =
        createValidMobSimConfig(targetHomeChargingShare = 1.5)

      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The target home charging share has to be between 0.0 and 1.0."
      )
    }

    "throw an exception when the charging hub threshold distance is negative" in {
      val invalidMobSimConfig = createValidMobSimConfig().copy(
        mobsim = createValidMobSimConfig().mobsim.copy(
          simulation = createValidMobSimConfig().mobsim.simulation.copy(
            location = Location(
              chargingHubThresholdDistance = -1.0,
              maxDistanceToChargingStation = 500.0,
              maxDistanceToHomeChargingStation = 30.0,
            )
          )
        )
      )

      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The charging hub threshold distance cannot be negative!"
      )
    }

    "throw an exception when maxDistanceToChargingStation is negative" in {
      val invalidMobSimConfig = createValidMobSimConfig().copy(
        mobsim = createValidMobSimConfig().mobsim.copy(
          simulation = createValidMobSimConfig().mobsim.simulation.copy(
            location = Location(
              chargingHubThresholdDistance = 50.0,
              maxDistanceToChargingStation = -10.0,
              maxDistanceToHomeChargingStation = 30.0,
            )
          )
        )
      )
      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The maximum permissible distance between a POI and a charging station cannot be negative!"
      )
    }

    "throw an exception when maxDistanceToHomeChargingStation is negative" in {
      val invalidMobSimConfig = createValidMobSimConfig().copy(
        mobsim = createValidMobSimConfig().mobsim.copy(
          simulation = createValidMobSimConfig().mobsim.simulation.copy(
            location = Location(
              chargingHubThresholdDistance = 50.0,
              maxDistanceToChargingStation = 500.0,
              maxDistanceToHomeChargingStation = -5.0,
            )
          )
        )
      )
      val exception = intercept[IllegalConfigException] {
        ConfigFailFast.check(invalidMobSimConfig)
      }
      exception.getMessage should include(
        "The maximum permissible distance between a home POI and a charging station cannot be negative!"
      )
    }

    "not throw an exception when all configurations are valid" in {
      val validMobSimConfig = createValidMobSimConfig()

      noException should be thrownBy ConfigFailFast.check(validMobSimConfig)
    }
  }
}
