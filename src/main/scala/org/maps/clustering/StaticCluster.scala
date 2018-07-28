package org.maps.clustering

import org.maps.clustering.clustering._

class StaticCluster[T <: ClusterItem](val center: GeoCoord, val hash: GeoHash) extends Cluster[T] {
  var items: Set[T] = Set.empty

  def add(t: T): Unit = {
    items += t
  }

  def position: GeoCoord = this.center

  def remove(t: T): Unit = {
    items -= t
  }

  def size: Int = items.size

  override def toString: String = "org.maps.clustering.StaticCluster{" + "mCenter=" + center + ", mItems.size=" + items.size + '}'

  override def hashCode: Int = this.center.hashCode + this.items.hashCode

  override def equals(other: Any): Boolean = {
    if (!other.isInstanceOf[StaticCluster[T]])
      return false

    val otherCluster = other.asInstanceOf[StaticCluster[T]]
    otherCluster.center.equals(this.center) && otherCluster.items.equals(this.items)
  }
}