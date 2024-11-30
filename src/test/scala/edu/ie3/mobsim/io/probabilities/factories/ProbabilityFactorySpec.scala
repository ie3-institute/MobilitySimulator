/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.probabilities.factories

import edu.ie3.test.common.UnitSpec

import scala.util.{Failure, Success, Try}

class ProbabilityFactorySpec
    extends UnitSpec
    with ProbabilityFactory[Seq[Map[String, String]]] {
  "Providing basic factory methods" should {
    "process a single line well" in {
      processLine("foo,Bar, baz", ",") should contain theSameElementsAs Seq(
        "foo",
        "bar",
        "baz",
      )
    }

    "accept a proper headline" in {
      checkHeadLine("foo,bar,baz", ",") match {
        case Failure(exception) =>
          fail("Failed, but was meant to succeed.", exception)
        case Success(value) =>
          value should contain theSameElementsAs Seq("foo", "bar", "baz")
      }
    }

    "refuse an improper headline" in {
      checkHeadLine("foo,bar,baz,whatsoever", ",") match {
        case Failure(exception) =>
          exception.getMessage shouldBe "Unable to read from content from file. Available headline elements: 'foo,bar,baz,whatsoever', required fields: 'foo,bar,baz'"
        case Success(value) =>
          fail(
            s"Was meant to fail, but succeeded with '${value.mkString("Array(", ", ", ")")}'."
          )
      }
    }
  }

  override protected val requiredFields: Seq[String] =
    Seq("foo", "bar", "baz")

  /** Build the desired instance from a collection of entity field data
    *
    * @param entityFieldData
    *   Collection of mappings from field to field value
    * @return
    *   A trial onto the instance
    */
  override protected def build(
      entityFieldData: Seq[Map[String, String]]
  ): Try[Seq[Map[String, String]]] = Try { entityFieldData }
}
