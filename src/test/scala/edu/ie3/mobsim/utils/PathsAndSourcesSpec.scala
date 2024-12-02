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

  "Paths and sources are built correctly" when {
    val basePath = Paths.get("").toAbsolutePath

    "using relative paths" in {

      val inputConfigRelative = Input(
        None,
        Grid("myGrid", CsvParams(",", "relativePath/grid")),
        Mobility(CsvParams(",", "relativePath/mobility")),
      )

      val actualRelative =
        PathsAndSources(
          "mySimulation",
          inputConfigRelative,
          Some("relativePath/results"),
        )

      val relPath = basePath.resolve("relativePath")

      actualRelative.mobSimInputDir shouldBe relPath
        .resolve("mobility")
        .toString
      actualRelative.outputDir shouldBe relPath.resolve("results").toString
    }

    "using absolute paths" in {

      val absPath = basePath.resolve("absolutePath")

      val inputConfigAbsolute = Input(
        None,
        Grid("myGrid", CsvParams(",", absPath.resolve("grid").toString)),
        Mobility(CsvParams(",", absPath.resolve("mobility").toString)),
      )

      val actualAbsolute =
        PathsAndSources(
          "mySimulation",
          inputConfigAbsolute,
          Some(absPath.resolve("results").toString),
        )

      actualAbsolute.mobSimInputDir shouldBe absPath
        .resolve("mobility")
        .toString
      actualAbsolute.outputDir shouldBe absPath.resolve("results").toString
    }

  }

}
