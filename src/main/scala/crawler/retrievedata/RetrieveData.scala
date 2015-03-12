package crawler.retrievedata

import crawler.FacebookConfig
import akka.actor.{ActorContext, Actor}
import akka.event.{Logging, LogSource}
import crawler.FacebookConfig.FacebookServiceConfig
import org.json4s.DefaultFormats
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}

import scala.concurrent.{Future, ExecutionContextExecutor}

/**
 * Created by roger on 05/03/15.
 */
object MyLogger {
  implicit val logSource: LogSource[AnyRef] = new LogSource[AnyRef] {
    def genString(o: AnyRef): String = o.getClass.getName
    override def getClazz(o: AnyRef): Class[_] = o.getClass
  }
}
abstract class RetrieveData extends Actor {
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher
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

