package org.maps.clustering

import scala.collection._

object clustering {

  case class Point(var x: Double, var y: Double)

  case class GeoCoord(var latitude: Double, var longitude: Double)

  case class GeoHash(prefix: Long, validBits: Int) {
    def makePrefix(d: Int): GeoHash = {
      val result = this.validBits - d
      GeoHash(this.prefix >> result, d)
    }

    def isCommonPrefix(other: GeoHash): Boolean = {
      if (this.validBits > other.validBits) {
        val shiftBits = this.validBits - other.validBits
        val shiftedPrefix: Long = this.prefix >> shiftBits
        shiftedPrefix == other.prefix
      } else {
        val shiftBits = other.validBits - this.validBits
        val shiftedPrefix: Long = other.prefix >> shiftBits
        shiftedPrefix == this.prefix
      }
    }

    def withSuffix(suffix: Int): GeoHash = {
      require(suffix < 4)
      GeoHash(prefix << 2 | suffix, validBits + 2)
    }

    def matchPrefix(other: GeoHash): Boolean = {
      if (this.validBits < other.validBits)
        false
      else
        (this.prefix >> (this.validBits - other.validBits)) == other.prefix
    }

    def truncatePrefix(hash: GeoHash): GeoHash = {
      require(this.validBits >= hash.validBits)
      val remainingBits = this.validBits - hash.validBits
      val mask: Long = (1L << remainingBits) - 1
      GeoHash(prefix & mask, remainingBits)
    }

    def cell: Int = {
      require(validBits >= 2)
      ((prefix >> (validBits - 2)) & 0x3L).toInt
    }

    def cellAfterPrefix(hash: GeoHash): Int = {
      require(validBits >= 2)
      val remainingBits = this.validBits - hash.validBits
      val mask: Long = (1L << remainingBits) - 1
      val truncated: Long = prefix & mask
      ((truncated >> (remainingBits - 2)) & 3L).toInt
    }

    override def toString: String = {
      val binary = java.lang.Long.toBinaryString(prefix | (1L << validBits)).substring(1)
      s"GeoHash($binary, $validBits)"
    }
  }

  case class Bounds(minX: Double, maxX: Double, minY: Double, maxY: Double) {
    final val midX: Double = (minX + maxX) / 2
    final val midY: Double = (minY + maxY) / 2

    def contains(point: Point): Boolean = contains(point.x, point.y)

    def contains(x: Double, y: Double): Boolean = this.minX <= x && x <= this.maxX && this.minY <= y && y <= this.maxY

    def contains(bounds: Bounds): Boolean = bounds.minX >= this.minX && bounds.maxX <= this.maxX && bounds.minY >= this.minY && bounds.maxY <= this.maxY

    def intersects(bounds: Bounds): Boolean = intersects(bounds.minX, bounds.maxX, bounds.minY, bounds.maxY)

    def intersects(_minX: Double, _maxX: Double, _minY: Double, _maxY: Double): Boolean =
      _minX < this.maxX && this.minX < _maxX && _minY < this.maxY && this.minY < _maxY
  }

  trait Algorithm[T <: ClusterItem] {
    def addItem(item: T): Unit

    def addItems(items: Vector[T]): Unit

    def clearItems(): Unit

    def removeItem(item: T): Unit

    def getClusters(zoom: Double): mutable.Set[_ <: Cluster[T]]

    def getItems: Set[T]
  }

  trait Tree[T <: Item] {
    def add(item: T): Unit

    def remove(item: T): Unit

    def clear(): Unit
  }

  trait Cluster[T <: ClusterItem] {
    def position: GeoCoord

    def hash: GeoHash

    def items: Set[T]

    def size: Int
  }

  trait Item {
    def point: Point

    def position: GeoCoord

    def hash: GeoHash
  }

  trait ClusterItem {
    def position: GeoCoord

    def title: String

    def snippet: String
  }

}
