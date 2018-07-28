package org.maps.app

import java.io.{BufferedReader, InputStreamReader}

import org.apache.commons.csv.{CSVFormat, CSVParser}
import org.maps.app.MapServiceProtocol.MapRequest
import org.maps.clustering.DistanceBasedClustering
import org.maps.clustering.clustering.{Cluster, ClusterItem, GeoCoord}

import scala.collection.JavaConverters._
import scala.collection.mutable

object StationMarkers {

  case class StationItem(var position: GeoCoord, var title: String, var snippet: String) extends ClusterItem

  def load(fileName: String): mutable.IndexedSeq[StationItem] = {
    var points = mutable.IndexedSeq[StationItem]()
    val resourceInputStream = ClassLoader.getSystemClassLoader.getResourceAsStream(fileName)
    val reader: BufferedReader = new BufferedReader(new InputStreamReader(resourceInputStream))
    val csvParser = new CSVParser(reader, CSVFormat.DEFAULT)

    try {
      for (csvRecord <- csvParser.getRecords.asScala) {
        if (csvRecord.getRecordNumber != 1L) {
          val latitude = csvRecord.get(9).toDouble
          val longitude = csvRecord.get(10).toDouble
          points = points :+ StationItem(GeoCoord(latitude, longitude), csvRecord.get(3), csvRecord.get(2))
        }
      }
      points
    } finally {
      if (reader != null)
        reader.close()
      if (csvParser != null)
        csvParser.close()
    }
  }
}

class StationMarkers(val fileName: String) {

  import StationMarkers._

  val items: mutable.IndexedSeq[StationItem] = load(fileName)

  def isVisible(neLat: Double, neLng: Double, swLat: Double, swLng: Double): StationItem => Boolean = {
    station => {
      val lng = station.position.longitude
      val lat = station.position.latitude
      swLat <= lat && lat <= neLat && swLng <= lng && lng <= neLng
    }
  }

  def getClusters(req: MapRequest): mutable.Set[_ <: Cluster[StationItem]] = {
    val visibleItems = items.filter(isVisible(req.neLat, req.neLng, req.swLat, req.swLng)).toVector
    val algo = new DistanceBasedClustering[StationItem](req.treeType)
    algo.addItems(visibleItems)
    algo.getClusters(req.zoomLevel)
  }

}
