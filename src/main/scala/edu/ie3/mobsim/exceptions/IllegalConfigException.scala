/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.exceptions

final case class IllegalConfigException(
    msg: String = "",
    cause: Throwable = None.orNull,
) extends Exception(msg, cause)
