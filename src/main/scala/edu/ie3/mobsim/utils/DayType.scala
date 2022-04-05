/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.utils

import scala.util.{Failure, Success, Try}

object DayType extends Enumeration {
  val WEEKDAY, SATURDAY, SUNDAY = Value

  def apply(token: String): Try[Value] = token.trim.toLowerCase match {
    case "weekday"  => Success(WEEKDAY)
    case "saturday" => Success(SATURDAY)
    case "sunday"   => Success(SUNDAY)
    case invalid =>
      Failure(
        new IllegalArgumentException(
          s"Unable to parse day type from unknown token '$invalid'. Permissible values: ${values
            .map(_.toString.toLowerCase)
            .mkString(", ")}"
        )
      )
  }
}
