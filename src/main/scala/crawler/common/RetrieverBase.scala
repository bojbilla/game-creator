package crawler.common


import akka.actor.{Actor, ActorLogging}
import crawler.FacebookConfig.FacebookServiceConfig
import org.json4s.DefaultFormats
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}

import scala.concurrent.Future

/**
 * Created by roger on 05/03/15.
 */

abstract class RetrieverBase extends Actor with ActorLogging{
  implicit def dispatcher =  context.dispatcher
  implicit def actorRefFactory = context
  implicit val formats = DefaultFormats
  def defaultFilter[T](entities: Vector[T]): Vector[T] = {
    for {
      e <- entities
    } yield e
  }

  def facebookPath = s"${FacebookServiceConfig.facebookHostAddress}"
  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )


}
