/*
 * © 2021. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim

import edu.ie3.simona.api.ExtLinkInterface
import edu.ie3.simona.api.simulation.{ExtSimAdapterData, ExtSimulation}

class ExtLink extends ExtLinkInterface {

  @Override
  override def getExtSimulation: ExtSimulation = MobilitySimulator

  @Override
  override def setup(extSimAdapterData: ExtSimAdapterData): Unit =
    MobilitySimulator.setAdapterData(extSimAdapterData)
}
