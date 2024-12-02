/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import edu.ie3.mobsim.model.TripSimulationTestData
import edu.ie3.test.common.UnitSpec
import squants.space.Meters

import java.util.UUID

class EvBuilderFromEvInputSpec extends UnitSpec with TripSimulationTestData {

  "Building evs" should {

    "work with EvInputs" in {
      /* Create 200 home POI, where 100 are available for home charging */
      val homePoisWithSizes = (Range(0, 100).map { cnt =>
        givenHomePoi.copy(
          id = s"home_poi_$cnt",
          nearestChargingStations = Map(
            givenChargingStation -> Meters(1.0)
          ),
        )
      } ++ Range(100, 200).map { cnt =>
        givenHomePoi.copy(
          id = s"home_poi_$cnt"
        )
      }).map(_ -> 1.0).toMap

      val evInputs =
        Range(0, 120).map(_ => evInput.copy().uuid(UUID.randomUUID()).build())

      forAll(
        Table(
          ("targetAmount", "targetShare", "expectedAmountOfHomeCharging"),
          (120, 1.0, 120),
          (120, 0.1, 12),
          (120, 0.12, 14),
        )
      ) {
        case (
              targetAmount: Int,
              targetShare: Double,
              expectedAmountOfHomeCharging: Int,
            ) =>
          val evs = EvBuilderFromEvInput.build(
            evInputs,
            homePoisWithSizes,
            givenWorkPoiPdf,
            Seq(givenChargingStation),
            givenSimulationStart,
            targetShare,
            givenFirstDepartureMetaData,
          )

          evs should have size targetAmount
          evs.count(
            _.chargingAtHomePossible
          ) shouldBe expectedAmountOfHomeCharging
      }
    }

  }

}
