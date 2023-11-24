/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.datamodel.models.{ElectricCurrentType, StandardUnits}
import edu.ie3.datamodel.models.input.system.`type`.chargingpoint.ChargingPointType
import edu.ie3.datamodel.models.input.system.`type`.evcslocation.EvcsLocationType
import edu.ie3.mobsim.io.geodata.PoiEnums.CategoricalLocationDictionary
import edu.ie3.mobsim.model.ChargingStation
import edu.ie3.util.quantities.PowerSystemUnits
import edu.ie3.util.quantities.QuantityUtils.RichQuantityDouble
import org.locationtech.jts.geom.Coordinate
import tech.units.indriya.ComparableQuantity
import tech.units.indriya.quantity.Quantities
import tech.units.indriya.unit.Units
import tech.units.indriya.unit.Units.{METRE, PERCENT}

import java.util.UUID
import javax.measure.quantity.Length

trait PoiTestData {
  private val csType = new ChargingPointType(
    "test",
    Quantities.getQuantity(11d, PowerSystemUnits.KILOVOLTAMPERE),
    ElectricCurrentType.AC
  )
  protected val cs0: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_0",
    new Coordinate(7.4116482, 51.4843281),
    csType,
    EvcsLocationType.HOME,
    1
  )
  protected val cs1: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_1",
    new Coordinate(7.4116472, 51.4843381),
    csType,
    EvcsLocationType.HOME,
    1
  )
  protected val cs2: ChargingStation = ChargingStation(
    UUID.fromString("7537c0b6-3137-4e30-8a95-db1c0f9d9b81"),
    "cs_2",
    new Coordinate(7.4115482, 51.4833281),
    csType,
    EvcsLocationType.STREET,
    1
  )
  protected val cs3: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_3",
    new Coordinate(7.41154872, 51.4833271),
    csType,
    EvcsLocationType.WORK,
    1
  )
  protected val cs4: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_4",
    new Coordinate(7.41153872, 51.4834271),
    csType,
    EvcsLocationType.CHARGING_HUB_TOWN,
    1
  )
  protected val cs5: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_5",
    new Coordinate(7.41153872, 51.4834271),
    csType,
    EvcsLocationType.HOME,
    1
  )
  protected val cs6: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_6",
    new Coordinate(7.41153842, 51.4834251),
    csType,
    EvcsLocationType.CHARGING_HUB_HIGHWAY,
    3
  )
  protected val cs7: ChargingStation = ChargingStation(
    UUID.randomUUID(),
    "cs_2",
    new Coordinate(7.4115482, 51.4833281),
    csType,
    EvcsLocationType.STREET,
    1
  )

  protected val poiHome: PointOfInterest = PointOfInterest(
    UUID.fromString("91b8a161-7e44-4363-aaa3-2d2adbaaab6d"),
    "POI_home_1",
    CategoricalLocationDictionary.HOME,
    new Coordinate(7.4116472, 51.4843381),
    25.557183061784293,
    Map(cs1 -> Quantities.getQuantity(0, METRE))
  )

  protected val workPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("a26b6850-c966-4b42-8ccf-762015bf19ba"),
    "work_617762273",
    CategoricalLocationDictionary.WORK,
    new Coordinate(7.41154872, 51.4833271),
    1549.4886151800551,
    Map(cs3 -> Quantities.getQuantity(0, METRE))
  )

  protected val bbpgPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("6e4e3fac-171c-471e-8ec7-cef0d172c39d"),
    "bbpg_88921333",
    CategoricalLocationDictionary.BBPG,
    new Coordinate(7.4622403, 51.5491056),
    2783.9202769435374,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val culturePoi: PointOfInterest = PointOfInterest(
    UUID.fromString("da6ffecd-301c-42cf-8985-402580985eae"),
    "culture_247900605",
    CategoricalLocationDictionary.CULTURE,
    new Coordinate(7.3339823, 51.5184387),
    6721.140094154159,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val medicinalPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("75f1f39a-28e5-44e4-85dd-c61e145b765f"),
    "medicinal_253820890",
    CategoricalLocationDictionary.MEDICINAL,
    new Coordinate(7.5112704, 51.5178795),
    4695.499976294351,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val otherShopPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("dda899fb-e085-479d-b3a9-cba594770a2b"),
    "other_shop_251193227",
    CategoricalLocationDictionary.OTHER_SHOP,
    new Coordinate(7.3682537, 51.4935477),
    1710.1059114060527,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val religiousPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("bdac04c2-c3d6-4a6c-97fb-812b57b7e008"),
    "religious_8654674",
    CategoricalLocationDictionary.RELIGIOUS,
    new Coordinate(7.5366142, 51.4726591),
    16984.79771274887,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val restaurantPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("02851eae-8014-4f87-895c-21d2b653b199"),
    "restaurant_268555314",
    CategoricalLocationDictionary.RESTAURANT,
    new Coordinate(7.4411392, 51.5097096),
    303.9431026,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val servicePoi: PointOfInterest = PointOfInterest(
    UUID.fromString("680df7d7-1c73-43ce-8eab-50d4a5105504"),
    "services_295406188",
    CategoricalLocationDictionary.SERVICES,
    new Coordinate(7.5862309, 51.5012326),
    548.2907601097887,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val sportsPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("7d04dc33-3a53-4857-b75e-7391fe468048"),
    "sports_292612919",
    CategoricalLocationDictionary.SPORTS,
    new Coordinate(7.416975, 51.5009765),
    12103.459075742423,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val supermarketPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("63dd665f-ca0b-4556-a4a9-bebf32ecdaf8"),
    "supermarket_261717630",
    CategoricalLocationDictionary.SUPERMARKET,
    new Coordinate(7.370586, 51.5234725),
    1498.211731553507,
    Map.empty[ChargingStation, ComparableQuantity[Length]]
  )

  protected val supermarket: PointOfInterest = PointOfInterest(
    UUID.fromString("0d50248c-eedd-4170-bbae-24bab064b121"),
    "supermarket_01",
    CategoricalLocationDictionary.SUPERMARKET,
    new Coordinate(7.4115482, 51.4833281),
    10424.542,
    Map(
      cs2 -> Quantities.getQuantity(0, METRE),
      cs7 -> Quantities.getQuantity(0, METRE)
    )
  )

  protected val chargingHubTownPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("4df0614d-0c01-4b31-af94-804e299f2686"),
    "charging_hub_town_261344967",
    CategoricalLocationDictionary.CHARGING_HUB_TOWN,
    new Coordinate(7.41153872, 51.4834271),
    129.211731553507,
    Map(cs4 -> Quantities.getQuantity(0, METRE))
  )
  protected val chargingHubHighwayPoi: PointOfInterest = PointOfInterest(
    UUID.fromString("3ddc93c7-77fc-4187-be68-833b3db39809"),
    "charging_hub_highway_261347306",
    CategoricalLocationDictionary.CHARGING_HUB_HIGHWAY,
    new Coordinate(7.41153842, 51.4834251),
    178.211731553507,
    Map(cs6 -> Quantities.getQuantity(0, METRE))
  )

  protected val itData: Seq[PointOfInterest] = Seq(
    poiHome,
    workPoi,
    bbpgPoi,
    culturePoi,
    medicinalPoi,
    otherShopPoi,
    religiousPoi,
    restaurantPoi,
    servicePoi,
    sportsPoi,
    supermarketPoi
  )
}
