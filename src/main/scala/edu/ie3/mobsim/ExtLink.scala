/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.simona.api.ExtLinkInterface
import edu.ie3.simona.api.data.SetupData
import edu.ie3.simona.api.simulation.ExtSimulation

class ExtLink extends ExtLinkInterface {

  @Override
  override def getExtSimulation: ExtSimulation = MobilitySimulator

  @Override
  override def setup(setupData: SetupData): Unit = {
    // no setup needed
  }
}
