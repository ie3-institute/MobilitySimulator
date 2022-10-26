/*
 * © 2022. TU Dortmund University,
 * Institute of Energy Systems, Energy Efficiency and Energy Economics,
 * Research group Distribution grid planning and operation
 */

package edu.ie3.mobsim.io.geodata

import edu.ie3.mobsim.config.MobSimConfig.CsvParams
import edu.ie3.test.common.UnitSpec

class HomePoiMappingSpec extends UnitSpec {

  "Home Poi mapping is read correctly" in {

    val filePath = this.getClass.getResource("poi_mapping.csv")
    val mappings = HomePoiMapping.readPois(CsvParams(filePath.getFile, ","))

    mappings.size shouldBe 2
    mappings match {
      case Seq(mappingA, _) =>
        mappingA.poi.toString shouldBe "6bf5830a-fac4-49bd-969b-e94af88e66fe"
        mappingA.evcs.toString shouldBe "8efd4be7-c499-46ff-a80f-cf69cf6bfbae"
        mappingA.evs.size shouldBe 2
        mappingA.evs match {
          case Seq(evA, evB) =>
            evA.toString shouldBe "f56225e4-3ef0-4f6f-a0b3-b0b5c8aafc8e"
            evB.toString shouldBe "6e528e93-0b31-455b-abc2-a0a5f1909aa2"
        }
    }
  }
}
