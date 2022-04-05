/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.simona.api;

import edu.ie3.mobsim.MobilitySimulator
import edu.ie3.simona.api.data.ExtDataSimulation
import edu.ie3.simona.api.simulation.ExtSimulation

import java.util

class ExtLink extends ExtLinkInterface {

  private val simulator: MobilitySimulator.type = MobilitySimulator

  @Override
  def getExtSimulation: ExtSimulation = {
    simulator
  }

  @Override
  def getExtDataSimulations: util.List[ExtDataSimulation] = {
    val list = new util.ArrayList[ExtDataSimulation]()
    list.add(simulator)
    list
  }

}
