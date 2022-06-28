/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.mobsim.io.probabilities.FirstDepartureOfDay
import edu.ie3.test.common.UnitSpec

import scala.util.{Failure, Success}

class FirstDepartureFactorySpec extends UnitSpec {
  "Getting first departure probabilities from file" when {
    "having a proper file" should {
      val properFilePath = this.getClass.getResource("departure.csv").getFile
      "pass" in {
        FirstDepartureFactory.getFromFile(properFilePath, ",") match {
          case Failure(exception) =>
            fail("Failed with exception, but was meant to pass.", exception)
          case Success(
                FirstDepartureOfDay(
                  probabilityWeekday,
                  probabilitySaturday,
                  probabilitySunday
                )
              ) =>
            probabilityWeekday.pdf should have size 1
            probabilitySaturday.pdf should have size 1
            probabilitySunday.pdf should have size 1
        }
      }
    }

    "having malformed files" should {
      "fail on malformed headline" in {
        val path =
          this.getClass.getResource("departure_headline_malformed.csv").getFile

        FirstDepartureFactory.getFromFile(path, ",") match {
          case Failure(exception) =>
            exception.getMessage shouldBe "Unable to read from content from file. Available headline elements: 'uuid,day_type,minute_of_day,something_weird', required fields: 'uuid,day_type,minute_of_day,probability'"
          case Success(value) =>
            fail(s"Should fail, but succeeded with '$value'.")
        }
      }

      "fail on unknown day type" in {
        val path =
          this.getClass.getResource("departure_unknown_day_type.csv").getFile

        FirstDepartureFactory.getFromFile(path, ",") match {
          case Failure(exception) =>
            exception.getMessage shouldBe "Unable to build first departure probabilities. First failure in stack trace."
            exception.getCause.getMessage shouldBe "Unable to parse day type from unknown token 'friyay'. Permissible values: saturday, sunday, weekday"
          case Success(value) =>
            fail(s"Should fail, but succeeded with '$value'.")
        }
      }
    }
  }
}
