package org.maps.clustering

import org.scalatest.Matchers._
import org.maps.clustering.clustering.GeoHash
import org.scalatest.WordSpec

class GeoHashSpec extends WordSpec {
  "GeoHash" should {
    "withSuffix" in {
      GeoHash(0, 0).withSuffix(3) shouldBe GeoHash(3, 2)
      GeoHash(3, 2).withSuffix(3) shouldBe GeoHash(15, 4)
    }

    "with too big Suffix" in {
      intercept[IllegalArgumentException] {
        GeoHash(3, 2).withSuffix(4)
      }
    }

    "truncatePrefix" in {
      GeoHash(0xF, 4).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x3, 2)
      GeoHash(0xE, 4).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x2, 2)
      GeoHash(0xD, 4).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x1, 2)
      GeoHash(0xC, 4).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x0, 2)
      GeoHash(0x1234, 16).truncatePrefix(GeoHash(0x12, 8)) shouldBe GeoHash(0x34, 8)
      GeoHash(0x1234, 16).truncatePrefix(GeoHash(0x1234, 16)) shouldBe GeoHash(0x0, 0)

      GeoHash(0xFFFFFFFFFFFFFFFFL, 64).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x3FFFFFFFFFFFFFFFL, 62)
      GeoHash(0xEFFFFFFFFFFFFFFFL, 64).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x2FFFFFFFFFFFFFFFL, 62)
      GeoHash(0xDFFFFFFFFFFFFFFFL, 64).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x1FFFFFFFFFFFFFFFL, 62)
      GeoHash(0xCFFFFFFFFFFFFFFFL, 64).truncatePrefix(GeoHash(0x3, 2)) shouldBe GeoHash(0x0FFFFFFFFFFFFFFFL, 62)
    }

    "truncatePrefix failure" in {
      intercept[IllegalArgumentException] {
        GeoHash(0x0, 0).truncatePrefix(GeoHash(0x0, 2))
      }
    }

    "cellAfterPrefix" in {
      GeoHash(0x1234, 16).cellAfterPrefix(GeoHash(0x12, 8)) shouldBe 0x0
      GeoHash(0x1274, 16).cellAfterPrefix(GeoHash(0x12, 8)) shouldBe 0x1
      GeoHash(0x1284, 16).cellAfterPrefix(GeoHash(0x12, 8)) shouldBe 0x2
      GeoHash(0x12C4, 16).cellAfterPrefix(GeoHash(0x12, 8)) shouldBe 0x3
    }

    "cellAfterPrefix failure" in {
      intercept[IllegalArgumentException] {
        GeoHash(0x0, 0).cellAfterPrefix(GeoHash(0x0, 2))
      }
    }

    "matchPrefix" in {
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0x0, 0)) shouldBe true
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0x0, 1)) shouldBe false
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0x2, 2)) shouldBe true
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0xC, 2)) shouldBe false
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0xA, 4)) shouldBe true
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0xA2, 8)) shouldBe true
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0xA23, 12)) shouldBe true
      GeoHash(0xA234, 16).matchPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA, 4).matchPrefix(GeoHash(0x1, 2)) shouldBe false
      GeoHash(0x1, 2).matchPrefix(GeoHash(0xA, 4)) shouldBe false
    }

    "isCommonPrefix" in {
      GeoHash(0x0, 0).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0x0, 0)) shouldBe true

      GeoHash(0x2, 2).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0x2, 2)) shouldBe true

      GeoHash(0xA, 4).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0xA, 4)) shouldBe true

      GeoHash(0xA2, 8).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0xA2, 8)) shouldBe true

      GeoHash(0xA23, 12).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0xA23, 12)) shouldBe true

      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
      GeoHash(0xA234, 16).isCommonPrefix(GeoHash(0xA234, 16)) shouldBe true
    }

    "cell" in {
      GeoHash(0x0, 2).cell shouldBe 0x0
      GeoHash(0x1, 2).cell shouldBe 0x1
      GeoHash(0x2, 2).cell shouldBe 0x2
      GeoHash(0x3, 2).cell shouldBe 0x3

      GeoHash(0x7232, 16).cell shouldBe 0x1

      GeoHash(0x3FFFFFFFFFFFFFFFL, 62).cell shouldBe 0x3
      GeoHash(0x2FFFFFFFFFFFFFFFL, 62).cell shouldBe 0x2
      GeoHash(0x1FFFFFFFFFFFFFFFL, 62).cell shouldBe 0x1
      GeoHash(0x0FFFFFFFFFFFFFFFL, 62).cell shouldBe 0x0
    }

    "cell failure" in {
      intercept[IllegalArgumentException] {
        GeoHash(0x0, 0).cell
      }
    }
  }
}
