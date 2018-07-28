package org.maps.clustering.projection

import org.maps.clustering.clustering._

final class MercatorProjection(val worldWidth: Double) {

  def toPoint(geoCoord: GeoCoord): Point = {
    var x: Double = geoCoord.longitude / 360 + .5
    val siny: Double = Math.sin(Math.toRadians(geoCoord.latitude))

    var y: Double = 0.5 * Math.log((1.0 + siny) / (1.0 - siny)) / -(2.0 * Math.PI) + .5

    x = replaceValue(x, Double.PositiveInfinity, +1)
    x = replaceValue(x, Double.NegativeInfinity, -1)
    y = replaceValue(y, Double.PositiveInfinity, +1)
    y = replaceValue(y, Double.NegativeInfinity, -1)

    Point(x * worldWidth, y * worldWidth)
  }

  def replaceValue(value: Double, bound: Double, toReplace: Double) : Double = {
    if (value == bound) {
      toReplace
    } else
      value
  }

  def toGeoCoord(point: Point): GeoCoord = {
    val x = point.x / worldWidth - 0.5
    val lng = x * 360
    val y = 0.5 - (point.y / worldWidth)
    val lat = 90 - Math.toDegrees(Math.atan(Math.exp(-y * 2 * Math.PI)) * 2)
    GeoCoord(lat, lng)
  }
}