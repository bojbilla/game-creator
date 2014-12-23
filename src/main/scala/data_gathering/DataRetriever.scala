package data_gathering

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import data_gathering.DataRetriever._
import database.DatabaseService
import service.GameRequest
import org.json4s.{DefaultFormats, Formats}
import spray.client.pipelining._
import spray.http.{HttpRequest, HttpResponse}
import spray.httpx.Json4sSupport
import spray.routing.HttpService
import scala.concurrent.ExecutionContext.Implicits.global

import scala.concurrent.Future

/**
 * Created by roger on 17/11/14.
 */

object DataRetriever {
  def props(): Props =
    Props(new DataRetriever())

  case class RetrieveData()
  case class RetrieveFBPages(gameRequest: GameRequest)
  case class RetrieveFBPosts(gameRequest: GameRequest)
  case class SentRetrieval(gameRequest: GameRequest)
  case class FailedToSendRetrieval(gameRequest: GameRequest)

}

class DataRetriever() extends HttpService with Actor with ActorLogging with Json4sSupport{
  def actorRefFactory: ActorContext = context
  implicit def json4sFormats: Formats = DefaultFormats

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive

  def receive = {
      case RetrieveFBPages(gameRequest) =>
        val pageRetriever = context.actorOf(FBRetriever.props(gameRequest))
        pageRetriever ! RetrieveData
      case RetrieveFBPosts(gameRequest) =>
        val pageRetriever = context.actorOf(FBRetriever.props(gameRequest))
        pageRetriever ! RetrieveData
      case _ => log.error("DataRetriever received unecpected message")
    }

  def awaitConfirmation: Receive = {
    case SentRetrieval(gameRequest) =>
      log.info(s"Sent retrieval for ${gameRequest.retrieveType}")
      sender ! Stop
    case FailedToSendRetrieval(gameRequest) =>
      log.error(s"Something could not be retrieved for: ${gameRequest.user_id}")
  }
}
