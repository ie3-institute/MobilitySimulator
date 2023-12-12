/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import edu.ie3.test.common.UnitSpec

class UtilsSpec extends UnitSpec {

  "Utils should round to quarter hour successfully" in {

    val cases = Table(
      ("time", "roundedTime"),
      (4, 15),
      (15, 15),
      (16, 15),
      (22, 15),
      (23, 30),
      (30, 30),
      (74, 75)
    )

    forAll(cases) { (time, roundedTime) =>
      {
        utils.roundToQuarterHourInMinutes(time) shouldBe roundedTime
      }
    }
  }

}
