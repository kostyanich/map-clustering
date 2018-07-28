package org.maps.clustering


import org.maps.clustering.DistanceBasedClustering.TreeItem
import org.maps.clustering.clustering.{ClusterItem, GeoCoord}
import org.maps.clustering.projection.{HilbertProjection, MercatorProjection}
import org.maps.clustering.tree.HilbertTree
import org.scalatest.Matchers._
import org.scalatest.WordSpec

class HilbertTreeSpec extends WordSpec {
  private val latitudeMin = -90
  private val latitudeMax = 90
  private val longitudeMin = -180
  private val longitudeMax = 180
  "HilbertTree" should {

    "create tree items and remove tree items for hash bits 2 to 62" in {

      for (dimBits <- 2 to 31) {
        val dim = scala.math.pow(2, dimBits)
        val projection = new MercatorProjection(dim)
        val hilbertProjection = new HilbertProjection(dimBits)
        var items = IndexedSeq[TreeItem[TestItem]]()
        val tree = new HilbertTree[TreeItem[TestItem]](dimBits * 2)
        for (lat <- latitudeMin to latitudeMax) {
          for (lon <- longitudeMin to longitudeMax) {
            val item = TreeItem(TestItem(GeoCoord(lat, lon), "", ""), projection, hilbertProjection)
            tree.add(item)
            items = items :+ item
          }
        }

        tree.size() shouldBe items.size

        items.foreach(item => tree.remove(item))
        tree.size() shouldBe 0
      }
    }

    "create tree and search for hash bits 4 to 64" in {

      for (dimBits <- 4 to 31) {
        println("dimBits: " + dimBits)
        val dim : Long = scala.math.pow(2, dimBits).asInstanceOf[Long]
        val projection = new MercatorProjection(dim)
        val hilbertProjection = new HilbertProjection(dimBits)
        var items = IndexedSeq[TreeItem[TestItem]]()
        val tree = new HilbertTree[TreeItem[TestItem]](dimBits * 2)
        for (lat <- latitudeMin to latitudeMax) {
          for (lon <- longitudeMin to longitudeMax) {
            val item = TreeItem(TestItem(GeoCoord(lat, lon), "", ""), projection, hilbertProjection)
            tree.add(item)
            items = items :+ item
          }
        }

        for (item <- items) {
          val set = tree.search(item.hash, None).map(_.position).toSet
          set should contain(item.position)
        }

        tree.size() shouldBe items.size

        items.foreach(item => tree.remove(item))
        tree.size() shouldBe 0
      }
    }

    "create tree and search for partial hash bits" in {

      for (dimBits <- 16 to 31) {
        println("dimBits: " + dimBits)
        val dim = scala.math.pow(2, dimBits)
        val projection = new MercatorProjection(dim)
        val hilbertProjection = new HilbertProjection(dimBits)
        var items = IndexedSeq[TreeItem[TestItem]]()
        val tree = new HilbertTree[TreeItem[TestItem]](dimBits * 2)
        for (lat <- latitudeMin to latitudeMax) {
          for (lon <- longitudeMin to longitudeMax) {
            val item = TreeItem(TestItem(GeoCoord(lat, lon), "", ""), projection, hilbertProjection)
            tree.add(item)
            items = items :+ item
          }
        }

        for (item <- items) {
          val searchHash = item.hash.makePrefix(dimBits / 2)
          val set = tree.search(searchHash, None).map(_.position).toSet
          set should contain(item.position)
        }

        tree.size() shouldBe items.size

        items.foreach(item => tree.remove(item))
        tree.size() shouldBe 0
      }
    }

  }

  case class TestItem(position: GeoCoord, title: String, snippet: String) extends ClusterItem

}
