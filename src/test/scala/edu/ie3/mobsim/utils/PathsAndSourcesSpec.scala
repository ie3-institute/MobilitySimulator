/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input
import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input.{Grid, Mobility}
import edu.ie3.test.common.UnitSpec

import java.nio.file.Paths

class PathsAndSourcesSpec extends UnitSpec {

  "Paths and sources are built correctly" in {
    val basePath = Paths.get("").toAbsolutePath.toString

    val inputConfigRelative = Input(
      Grid("myGrid", CsvParams(",", "relativePath/grid")),
      Mobility(CsvParams(",", "relativePath/mobility"))
    )
    val inputConfigAbsolute = Input(
      Grid("myGrid", CsvParams(",", "relativePath/grid")),
      Mobility(CsvParams(",", "/absolutePath/mobility"))
    )

    val actualRelative =
      PathsAndSources(
        "mySimulation",
        inputConfigRelative,
        Some("relativePath/results")
      )
    val actualAbsolute =
      PathsAndSources(
        "mySimulation",
        inputConfigAbsolute,
        Some("/absolutePath/results")
      )

    actualRelative.mobSimInputDir shouldBe Paths
      .get(basePath, "relativePath", "mobility")
      .toString
    actualRelative.outputDir shouldBe Paths
      .get(basePath, "relativePath", "results")
      .toString
    actualAbsolute.mobSimInputDir shouldBe Paths
      .get("/absolutePath", "mobility")
      .toString
    actualAbsolute.outputDir shouldBe Paths
      .get("/absolutePath", "results")
      .toString
  }

}
