import akka.actor.typed.ActorSystem
import akka.actor.typed.scaladsl.Behaviors
import akka.http.scaladsl.Http
import akka.http.scaladsl.marshallers.sprayjson.SprayJsonSupport._
import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.Route
import akka.util.ByteString
import spray.json.DefaultJsonProtocol

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.control.NonFatal

/**
 * Run script and try
 *
 *  `$ curl http://localhost:8080/fetch\?url\=https://akka.com/\&url\=http://icanhazip.com\&url\=http://doesntexits.com`
 */
object Main {

  import DefaultJsonProtocol._

  implicit val system = ActorSystem(Behaviors.empty, "simple-crawler")
  implicit val executionContext = system.executionContext
  implicit val pageInfoFormat = jsonFormat3(PageInfo)

  val titleRegex = "<title>([^<]+)</title>".r("content")

  def route(): Route = (path("fetch") & parameter('url.repeated)) { urls =>
    get {
      urls.toList.map(_.trim).filter(_.nonEmpty) match {
        case Nil =>
          complete("Specify at least one url param")
        case urls =>
          complete(crawl(urls))
      }
    }
  }

  def crawl(url: String): Future[PageInfo] = {
    Http().singleRequest(HttpRequest(uri = url)).flatMap { response =>
      response.entity.dataBytes.runFold(ByteString(""))(_ ++ _).map { body =>
        val title = titleRegex.findFirstMatchIn(body.utf8String).map(_.group("content"))
        PageInfo(url, title, None)
      }
    }
  }

  def crawl(urls: List[String]): Future[List[PageInfo]] = {
    Future.sequence {
      urls.map { url =>
        crawl(url).recover {
          case NonFatal(e) => PageInfo(url, None, Some(e.toString))
        }
      }
    }
  }

  def main(args: Array[String]): Unit = {
    val bindingFuture = Http()
      .newServerAt("localhost", 8080)
      .bind(route())

    println(s"Server online at http://localhost:8080")
    println("Press RETURN to stop...")
    StdIn.readLine()

    bindingFuture
      .flatMap(_.unbind())
      .onComplete(_ => system.terminate())
  }
}

final case class PageInfo(url: String, title: Option[String], error: Option[String])
