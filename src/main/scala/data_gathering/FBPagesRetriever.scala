package data_gathering

import akka.actor.{Props, ActorContext, ActorLogging, Actor}
import data_gathering.DataRetriever._
import database.DatabaseService
import org.json4s.{DefaultFormats, Formats}
import server.Server
import service.GameRequest
import spray.client.pipelining._
import spray.http.{HttpResponse, HttpRequest}
import spray.http.StatusCodes._
import spray.httpx.Json4sSupport
import spray.routing.HttpService
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future
import scala.util.{Failure, Success}

object FBPagesRetriever{
  def props(gameRequest: GameRequest): Props =
    Props(new FBPagesRetriever(gameRequest))
}

/**
 * Created by roger on 17/11/14.
 */
class FBPagesRetriever(gameRequest: GameRequest) extends HttpService with Actor with ActorLogging with Json4sSupport{
  def actorRefFactory: ActorContext = context
  implicit def json4sFormats: Formats = DefaultFormats

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  def receive = {
    case RetrieveData =>
      val client = sender()
      val token = gameRequest.access_token
      val user_id = gameRequest.user_id
      val return_address = s"http://${Server.hostName}:${Server.port}/liked_pages/$user_id"

      val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
      val responseF = pipeline(Get(gameRequest.crawler_address))
      responseF.onComplete{
        r => log.info("we received " + r.get.status)
          r match {
            case Success(response) =>
              if (response.status == OK) {
                client ! SentRetrieval
              } else {
                log.error(s"Unable to retrieve pages for $user_id")
                client ! FailedToSendRetrieval
              }
            case Failure(e) =>
              log.error(s"Unable to retrieve pages for $user_id")
              client ! FailedToSendRetrieval
          }
      }

  }
}
