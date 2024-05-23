/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.model.builder

import edu.ie3.mobsim.io.probabilities.ProbabilityDensityFunction
import edu.ie3.mobsim.model.{ElectricVehicle, TripSimulationTestData}
import edu.ie3.test.common.UnitSpec
import squants.Meters

class EvBuilderFromRandomModelSpec
    extends UnitSpec
    with TripSimulationTestData {

  "The builder" when {

    /* Building with random attributes is not tested, because it is assumed, that the random processes are tested
     * separately. Other than that, there is no additional logic in the method. */

    "assigning the first cars" should {
      val assignInitialHomeChargingCars =
        PrivateMethod[Iterable[ElectricVehicle]](
          Symbol("assignInitialHomeChargingCars")
        )

      "provide the correct amount of home charging cars" in {
        forAll(
          Table(
            ("amountOfHomeChargingCars", "expectedAmount"),
            (0, 0),
            (10, 10),
            (100, 100),
            (120, 100)
          )
        ) { case (amountOfHomeChargingCars: Int, expectedAmount: Int) =>
          val initialCars =
            EvBuilderFromRandomModel invokePrivate assignInitialHomeChargingCars(
              amountOfHomeChargingCars,
              ProbabilityDensityFunction(
                Range(0, 100)
                  .map(cnt => givenHomePoi.copy(id = s"home_poi_$cnt") -> 1.0)
                  .toMap
              ),
              givenWorkPoiPdf,
              givenModelPdf,
              givenFirstDepartureMetaData,
              givenSimulationStart
            )
          /* There are 100 home charging stations available. The actual amount of cars is limited by the minimum of
           * their target amount and the amount of available home POI */
          initialCars.size shouldBe expectedAmount
        }
      }
    }

    "assigning the remaining cars" should {
      val testCases = Table(
        (
          "amountOfEvsInArea",
          "amountOfHomeChargingCars",
          "amountOfAssignedCars",
          "expectedHomeChargingAmount",
          "expectedOverallAmount"
        ),
        (100, 0, 0, 0, 100), // All cars already assigned
        (100, 10, 10, 0, 90),
        (100, 100, 100, 0, 0),
        (100, 0, 10, 0, 90), // Too many already assigned
        (100, 10, 20, 0, 80),
        (100, 100, 110, 0, 0),
        (100, 30, 10, 20, 90)
      )

      "determine the amount of needed cars correctly" in {
        val determineUnassignedCars =
          PrivateMethod[(Int, Int)](Symbol("determineUnassignedCars"))

        forAll(testCases) {
          case (
                amountOfEvsInArea: Int,
                amountOfHomeChargingCars: Int,
                amountOfAssignedCars: Int,
                expectedHomeChargingAmount: Int,
                expectedOverallAmount: Int
              ) =>
            EvBuilderFromRandomModel invokePrivate determineUnassignedCars(
              amountOfEvsInArea,
              amountOfHomeChargingCars,
              amountOfAssignedCars
            ) match {
              case (amountOfHomeCharging, overallAmount) =>
                amountOfHomeCharging shouldBe expectedHomeChargingAmount
                overallAmount shouldBe expectedOverallAmount
            }
        }
      }

      "provide the correct portion of cars with and without home charging option" in {
        val assignRemainingCars = PrivateMethod[Iterable[ElectricVehicle]](
          Symbol("assignRemainingCars")
        )
        forAll(testCases) {
          case (
                amountOfEvsInArea: Int,
                amountOfHomeChargingCars: Int,
                amountOfAssignedCars: Int,
                expectedHomeChargingAmount: Int,
                expectedOverallAmount: Int
              ) =>
            val additionalCars =
              EvBuilderFromRandomModel invokePrivate assignRemainingCars(
                amountOfEvsInArea,
                amountOfHomeChargingCars,
                amountOfAssignedCars,
                givenHomePoiPdf,
                givenHomePoiPdf,
                givenWorkPoiPdf,
                givenModelPdf,
                givenFirstDepartureMetaData,
                givenSimulationStart
              )

            additionalCars should have size expectedOverallAmount
            additionalCars.count(
              _.chargingAtHomePossible
            ) shouldBe expectedHomeChargingAmount
        }
      }
    }

  }

  "creating the cars" should {
    "provide the correct amount and portion" in {
      /* Create 200 home POI, where 100 are available for home charging */
      val homePoisWithSizes = (Range(0, 100).map { cnt =>
        givenHomePoi.copy(
          id = s"home_poi_$cnt",
          nearestChargingStations = Map(
            givenChargingStation -> Meters(1.0)
          )
        )
      } ++ Range(100, 200).map { cnt =>
        givenHomePoi.copy(
          id = s"home_poi_$cnt"
        )
      }).map(_ -> 1.0).toMap

      forAll(
        Table(
          ("targetAmount", "targetShare", "expectedAmountOfHomeCharging"),
          (10, 1.0, 10),
          (100, 1.0, 100),
          (100, 0.1, 10),
          (100, 0.125, 13)
        )
      ) {
        case (
              targetAmount: Int,
              targetShare: Double,
              expectedAmountOfHomeCharging: Int
            ) =>
          val evs = EvBuilderFromRandomModel.build(
            targetAmount,
            homePoisWithSizes,
            givenWorkPoiPdf,
            Seq(givenChargingStation),
            givenSimulationStart,
            targetShare,
            givenModelPdf,
            givenFirstDepartureMetaData
          )

          evs should have size targetAmount
          evs.count(
            _.chargingAtHomePossible
          ) shouldBe expectedAmountOfHomeCharging
      }
    }
  }
}
