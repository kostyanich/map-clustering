package org.maps.clustering.projection

import org.maps.clustering.clustering.{GeoHash, Point}

final class HilbertProjection(val worldSizeBits: Int) {
  val validBits: Int = worldSizeBits * 2
  val worldDimension: Long = scala.math.pow(2, this.worldSizeBits).toLong

  def toGeoHash(point: Point): GeoHash = {
    var rx: Long = 0L
    var ry: Long = 0L
    var result: Long = 0
    val xy = new Array[Long](2)
    xy(0) = Math.round(point.x)
    xy(1) = Math.round(point.y)
    var s = worldDimension / 2

    while (s > 0) {
      rx = if ((xy(0) & s) > 0) 1 else 0
      ry = if ((xy(1) & s) > 0) 1 else 0
      result += s * s * ((3 * rx) ^ ry)

      rot(s, xy, rx, ry)
      s /= 2
    }
    GeoHash(result, this.validBits)
  }


  def toPoint(hash: GeoHash): Point = {
    var rx: Long = 0L
    var ry: Long = 0L
    var t: Long = hash.prefix
    val xy = new Array[Long](2)
    xy(0) = 0
    xy(1) = 0
    var s: Long = 1
    while (s < this.worldDimension) {
      rx = 1 & (t / 2)
      ry = 1 & (t ^ rx)
      rot(s, xy, rx, ry)
      xy(0) += s * rx
      xy(1) += s * ry
      t /= 4
      s *= 2
    }

    Point(xy(0).longValue(), xy(1).longValue())
  }

  def rot(d: Long, xy: Array[Long], rx: Long, ry: Long): Unit = {
    if (ry == 0) {
      if (rx == 1) {
        xy(0) = d - 1 - xy(0)
        xy(1) = d - 1 - xy(1)
      }

      //Swap x and y
      val t = xy(0)
      xy(0) = xy(1)
      xy(1) = t
    }
  }

}
