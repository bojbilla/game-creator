package me.reminisce.service

import akka.actor._
import me.reminisce.crawler.CrawlerService
import me.reminisce.crawler.CrawlerService.FetchData
import me.reminisce.server.domain.RestMessage
import me.reminisce.server.domain.resthandling.PerRequestCreator
import me.reminisce.service.GameGenerator.CreateBoard
import me.reminisce.service.questiongen.QuestionGenerator.CreateQuestion
import me.reminisce.service.questiongen.{WhenDidYouShareThisPost, WhichPageDidYouLike, WhoLikedYourPost, WhoMadeThisCommentOnYourPost}
import reactivemongo.api.DefaultDB
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse}
import spray.httpx.Json4sSupport
import spray.routing._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

/**
 * Created by roger on 15/11/14.
 */

object GameCreatorService {

  case class CrawlerTest(message: String)

}

trait GameCreatorServiceActor extends GameCreatorService {
  def actorRefFactory = context

  def receive = runRoute(gameCreatorRoutes)
}

trait GameCreatorService extends HttpService with PerRequestCreator with Actor with ActorLogging with Json4sSupport {
  def actorRefFactory: ActorContext


  val db: DefaultDB
  val dataRetriever: ActorRef

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )
  val gameCreatorRoutes = {

    path("fetchData") {
      get {
        parameters('user_id.as[String], 'access_token.as[String]) {
          (user_id: String, access_token: String) =>
            fetchData {
              FetchData(user_id, access_token)
            }
        }
      }
    } ~ path("which_page_did_you_like") {
      get {
        parameters('user_id.as[String]) {
          (user_id: String) =>
            whichPageDidYouLike {
              CreateQuestion(user_id)
            }
        }
      }
    } ~ path("gameboard") {
      parameters('user_id.as[String]) { user_id: String =>
        createBoard {
          CreateBoard(user_id)
        }
      }
    }
  }

  def whichPageDidYouLike(message: RestMessage): Route = {
    val generator = context.actorOf(WhichPageDidYouLike.props(db))
    ctx => perRequest(ctx, generator, message)
  }

  def whenDidYouShareThisPost(message: RestMessage): Route = {
    val generator = context.actorOf(WhenDidYouShareThisPost.props(db))
    ctx => perRequest(ctx, generator, message)
  }

  def whoMadeThisCommentOnYourPost(message: RestMessage): Route = {
    val generator = context.actorOf(WhoMadeThisCommentOnYourPost.props(db))
    ctx => perRequest(ctx, generator, message)
  }

  def whoLikedPost(message: RestMessage): Route = {
    val generator = context.actorOf(WhoLikedYourPost.props(db))
    ctx => perRequest(ctx, generator, message)
  }

  def tryToCreateBoard(message: RestMessage): Route = {
    val generator = context.actorOf(GameGenerator.props(db))
    ctx => perRequest(ctx, generator, message)
  }


  def createBoard(message: RestMessage): Route = {
    log.info("Creating game board")
    val generator = context.actorOf(GameGenerator.props(db))
    ctx => perRequest(ctx, generator, message)
  }

  def fetchData(message: RestMessage): Route = {
    ctx => perRequest(ctx, dataRetriever, message)
  }

}
