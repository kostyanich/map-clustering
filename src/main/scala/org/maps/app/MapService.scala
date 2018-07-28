package org.maps.app

import akka.actor.ActorSystem
import akka.event.{Logging, LoggingAdapter}
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.server.Directives._
import akka.stream.{ActorMaterializer, Materializer}
import com.typesafe.config.{Config, ConfigFactory}
import spray.json.{DefaultJsonProtocol, _}

import scala.concurrent.{ExecutionContextExecutor, Future}

sealed trait TreeType
case class QuadTreeType() extends TreeType
case class HilbertTreeType() extends TreeType

trait MapServiceProtocol extends DefaultJsonProtocol {
  case class MapRequest(zoomLevel: Int, neLat: Double, neLng: Double, swLat: Double, swLng: Double, treeType: TreeType)
  case class Marker(latitude: Double, longitude: Double, count: Int)

  implicit val treeTypeFormat = new RootJsonFormat[TreeType] {
    override def write(obj: TreeType): JsValue = JsString(obj.getClass.getName)
    override def read(json: JsValue): TreeType = json match {
      case JsString(value) if value == classOf[QuadTreeType].getSimpleName => QuadTreeType()
      case JsString(value) if value == classOf[HilbertTreeType].getSimpleName => HilbertTreeType()
    }
  }
  implicit val mapFormat : RootJsonFormat[MapRequest] = jsonFormat6(MapRequest.apply)
  implicit val markerFormat = jsonFormat3(Marker.apply)
}

object MapServiceProtocol extends MapServiceProtocol

trait Service  {
  import MapServiceProtocol._
  implicit val system: ActorSystem

  implicit def executor: ExecutionContextExecutor

  implicit val materializer: Materializer

  def config: Config

  val logger: LoggingAdapter

  val markers = new StationMarkers("ocm_all.csv")

  val routes = {
    logRequestResult("map-service") {
      pathPrefix("map") {
        post {
          entity(as[MapRequest]) { req: MapRequest =>
            complete {
              Future {
                markers.getClusters(req).map { cluster =>
                  Marker(cluster.position.latitude, cluster.position.longitude, cluster.size)
                }.toList
              }
            }
          }
        }
      } ~
        getFromResource("index.html")
    }
  }
}

object MapService extends App with Service {
  override implicit val system = ActorSystem()
  override implicit val executor = system.dispatcher
  override implicit val materializer = ActorMaterializer()

  override val config = ConfigFactory.load()
  override val logger = Logging(system, getClass)

  Http().bindAndHandle(routes, "0.0.0.0", 8080).map(_ => println("Server Running..."))
}