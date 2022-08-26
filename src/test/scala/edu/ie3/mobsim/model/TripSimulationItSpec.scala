/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model

import edu.ie3.mobsim.config.MobSimConfig.Mobsim.Input
import edu.ie3.mobsim.utils.{IoUtils, PathsAndSources}
import edu.ie3.test.common.UnitSpec

class TripSimulationItSpec extends UnitSpec {

//  currentTime: ZonedDateTime,
//  ev: ElectricVehicle,
//  poisWithSizes: Map[
//    CategoricalLocationDictionary.Value,
//    ProbabilityDensityFunction[PointOfInterest]
//  ],
//  chargingHubTownIsPresent: Boolean,
//  chargingHubHighwayIsPresent: Boolean,
//  chargingStations: Seq[ChargingStation],
//  ioUtils: IoUtils,
//  tripProbabilities: TripProbabilities,
//  thresholdChargingHubDistance: ComparableQuantity[Length]

  "Simulate the trips correctly" in {

    val inputConfig = Input()

    val pathsAndSources = PathsAndSources()

    val ioUtils = IoUtils(
      pathsAndSources.outputDir,
      "movements.csv",
      "evs.csv",
      "evcs.csv",
      "positions.csv",
      "pois.csv"
    )

    TripSimulation.simulateNextTrip()
  }

}
