package org.maps.clustering

import org.maps.app.{HilbertTreeType, QuadTreeType, TreeType}
import org.maps.clustering.clustering.{ClusterItem, Point, _}
import org.maps.clustering.projection.{HilbertProjection, MercatorProjection}
import org.maps.clustering.tree.{HilbertTree, QuadTree}

import scala.collection._

object DistanceBasedClustering {
  private val DEFAULT_MAX_DISTANCE_AT_ZOOM = 100

  case class TreeItem[T <: ClusterItem](clusterItem: T, projection: MercatorProjection, hilbertProjection: HilbertProjection)
    extends Item with Cluster[T] {
    val position: GeoCoord = clusterItem.position
    val point: Point = projection.toPoint(position)
    val hash: GeoHash = hilbertProjection.toGeoHash(point)
    val items: Set[T] = Set(clusterItem)

    override def size = 1

    override def hashCode: Int = clusterItem.hashCode

    override def equals(other: Any): Boolean = {
      if (!other.isInstanceOf[TreeItem[T]])
        return false
      other.asInstanceOf[TreeItem[T]].clusterItem.equals(this.clusterItem)
    }
  }

}

class DistanceBasedClustering[T <: ClusterItem](treeType: TreeType) extends Algorithm[T] {

  import DistanceBasedClustering._

  private val dimBits = 11
  private val dimension: Long = scala.math.pow(2, dimBits).asInstanceOf[Int]
  private var maxDistance: Long = dimension * DEFAULT_MAX_DISTANCE_AT_ZOOM
  private val hilbertProjection = new HilbertProjection(dimBits)
  private val mercatorProjection = new MercatorProjection(dimension)

  private var items = mutable.LinkedHashSet[TreeItem[T]]()
  private val indexTree: Tree[TreeItem[T]] = treeType match {
    case HilbertTreeType() => new HilbertTree(dimBits * 2)
    case QuadTreeType()    => new QuadTree[TreeItem[T]](Bounds(0, dimension, 0, dimension))
  }

  override def addItem(item: T): Unit = {
    val quadItem = TreeItem[T](item, mercatorProjection, hilbertProjection)
    items += quadItem
    indexTree.add(quadItem)
  }

  def addItems(items: Vector[T]): Unit = {
    for (item <- items)
      addItem(item)
  }

  override def clearItems(): Unit = {
    items = mutable.LinkedHashSet[TreeItem[T]]()
    indexTree.clear()
  }

  override def removeItem(item: T): Unit = {
    val quadItem = TreeItem[T](item, mercatorProjection, hilbertProjection)
    items.remove(quadItem)
    indexTree.remove(quadItem)
  }

  override def getClusters(zoom: Double): mutable.Set[_ <: clustering.Cluster[T]] = {
    val discreteZoom = zoom.toInt
    val zoomSpecificSpan: Double = maxDistance / Math.pow(2, discreteZoom) / 256L

    var visitedCandidates = mutable.Set[TreeItem[T]]()
    var results = mutable.Set[clustering.Cluster[T]]()
    var distanceToCluster = mutable.Map[TreeItem[T], Double]()
    var itemToCluster = mutable.Map[TreeItem[T], StaticCluster[T]]()

    for (candidate <- getClusteringItems(indexTree, discreteZoom)) {
      if (!visitedCandidates.contains(candidate)) { // Candidate is already part of another cluster.'
        val clusterItems: Seq[TreeItem[T]] = getNeighbours(candidate, zoomSpecificSpan, discreteZoom)
        if (clusterItems.size == 1) { // Only the current marker is in range. Just add the single item to the results.
          results += candidate
          visitedCandidates += candidate
          distanceToCluster += candidate -> 0d
        } else {
          val cluster = new StaticCluster[T](candidate.clusterItem.position, candidate.hash)
          results += cluster

          for (clusterItem <- clusterItems) {
            val existingDistance: Option[Double] = distanceToCluster.get(clusterItem)
            val distance = distanceSquared(clusterItem.point, candidate.point)
            val alreadyBelongToAnotherCluster = existingDistance.isDefined && existingDistance.get < distance
            if (!alreadyBelongToAnotherCluster) {
              if (existingDistance.isDefined && existingDistance.get >= distance) {
                // Move item to the closer cluster.
                itemToCluster(clusterItem).remove(clusterItem.clusterItem)
              }
              distanceToCluster += clusterItem -> distance
              cluster.add(clusterItem.clusterItem)
              itemToCluster += clusterItem -> cluster
            }
          }
          visitedCandidates ++= clusterItems
        }
      }
    }

    results
  }

  protected def getNeighbours(candidate: TreeItem[T], zoomSpecificSpan: Double, discreteZoom: Int): Seq[TreeItem[T]] = {
    val searchBounds = createBoundsFromSpan(candidate.point, zoomSpecificSpan)
    indexTree match {
      case tree: QuadTree[TreeItem[T]] => tree.search(searchBounds)
      case tree: HilbertTree[TreeItem[T]] =>
//        val bits = Math.ceil(Math.log(zoomSpecificSpan) / Math.log(2)).toInt + 1
        val prefix = discreteZoom
        val hash = hilbertProjection.toGeoHash(candidate.point).makePrefix(prefix)
        tree.search(hash, Some(searchBounds))
    }
  }

  protected def getClusteringItems(quadTree: Tree[TreeItem[T]], discreteZoom: Int): Vector[TreeItem[T]] = items.toVector

  override def getItems: Set[T] = {
    var result = Set[T]()
    for (quadItem <- items) {
      result += quadItem.clusterItem
    }
    result
  }

  private def distanceSquared(a: Point, b: Point): Double = (a.x - b.x) * (a.x - b.x) + (a.y - b.y) * (a.y - b.y)

  private def createBoundsFromSpan(p: Point, span: Double): Bounds = {
    val halfSpan = span / 2
    Bounds(p.x - halfSpan, p.x + halfSpan, p.y - halfSpan, p.y + halfSpan)
  }
}