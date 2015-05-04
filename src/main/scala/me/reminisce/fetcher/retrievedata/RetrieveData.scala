package me.reminisce.fetcher.retrievedata

import akka.actor.{Actor, ActorContext}
import akka.event.LogSource
import me.reminisce.fetcher.FacebookServiceConfig
import org.json4s.DefaultFormats
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse}

import scala.concurrent.{ExecutionContextExecutor, Future}

object MyLogger {
  implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
    def genString(o: AnyRef): String = o.getClass.getName

    override def getClazz(o: AnyRef): Class[_] = o.getClass
  }
}

abstract class RetrieveData extends Actor {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  import MyLogger._
  import akka.event.Logging

  val log = Logging(context.system, this)
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

