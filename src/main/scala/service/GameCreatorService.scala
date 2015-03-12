package service

import akka.actor._
import akka.event.Logging
import crawler.CrawlerService.FetchData
import crawler.common.GraphResponses
import crawler.common.GraphResponses.{Post, Page}
import database.MongoDatabaseService
import database.MongoDatabaseService.{SaveFBTaggedPost, SaveFBPost, SaveFBPage}
import routing.{PerRequest, PerRequestCreator}
import server.domain.RestMessage
import mongodb.MongoDBEntities.{FBTag, FBPhoto, FBPage}
import reactivemongo.api.indexes.{Index, CollectionIndexesManager}
import reactivemongo.api.{MongoConnection, MongoDriver, DefaultDB}
import service.GameGenerator.{CreateBoard}
import service.question_generators.QuestionGenerator.CreateQuestion
import service.question_generators.{WhenDidYouShareThisPost, WhoMadeThisCommentOnYourPost, WhoLikedYourPost}
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}
import spray.httpx.Json4sSupport
import spray.routing._
import server.Server
import spray.http.StatusCode
import spray.http.StatusCodes._
import scala.concurrent.ExecutionContext.Implicits.global


import scala.concurrent.Future

/**
 * Created by roger on 15/11/14.
 */

object GameCreatorService{
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
              fetchData{
                FetchData(user_id, access_token)
              }
        }
      }
    } ~ path("liked_pages" / Segment) { user_id =>
      post {
        {
          entity(as[List[Page]]) { pages =>
            complete{
              log.info(s"Received liked_pages from crawler for $user_id")
              val mongoSaver = context.actorOf(MongoDatabaseService.props(user_id, db))
              mongoSaver ! SaveFBPage(pages)
              OK
            }
          }
        }
      }
    } ~ path("tagged_posts" / Segment) { user_id =>
      post {
        entity(as[List[Post]]) { posts =>
          complete{
            log.info(s"Received tagged_posts from crawler for $user_id")
            val mongoSaver = context.actorOf(MongoDatabaseService.props(user_id, db))
            mongoSaver ! SaveFBTaggedPost(posts)
            OK
          }
        }
      }

    } ~ path("posts" / Segment) { user_id =>
      post {
        entity(as[List[Post]]) { posts =>
          complete {
            log.info(s"Received posts from crawler for $user_id")
            val mongoSaver = context.actorOf(MongoDatabaseService.props(user_id, db))
            mongoSaver ! SaveFBPost(posts)
            OK
          }
        }
      }

    } ~ path("gameboard") {
      parameters('user_id.as[String]) { user_id: String =>
        createBoard{
          CreateBoard(user_id)
        }
      }
    }
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

  def retrieveFBPages(user_id: String, access_token: String): Unit = {
    val returnAddress = s"http://${Server.hostName}:${Server.port}/liked_pages/$user_id"
    val crawlerAddress = s"${Server.fullCrawlerHost}/liked_pages?user_id=$user_id&access_token=$access_token&return_address=$returnAddress"
    val gameRequest = GameRequest(user_id, access_token, returnAddress, crawlerAddress, Some(GameRequest.fbPageType))
//    dataRetriever ! RetrieveFBPages(gameRequest)
  }

  def retrieveFBTaggedPosts(user_id: String, access_token: String): Unit = {
    log.info(s"retrieving FBTaggedPosts for $user_id")
    val returnAddress = s"http://${Server.hostName}:${Server.port}/tagged_posts/$user_id"
    val crawlerAddress = s"${Server.fullCrawlerHost}/tagged_posts?user_id=$user_id&access_token=$access_token&return_address=$returnAddress"
    val gameRequest = GameRequest(user_id, access_token, returnAddress, crawlerAddress, Some(GameRequest.fbPostType))
//    dataRetriever ! RetrieveFBPosts(gameRequest)
  }

  def retrieveFBPosts(user_id: String, access_token: String): Unit = {
    log.info(s"retrieving FBPosts for $user_id")
    val returnAddress = s"http://${Server.hostName}:${Server.port}/posts/$user_id"
    val crawlerAddress = s"${Server.fullCrawlerHost}/posts?user_id=$user_id&access_token=$access_token&return_address=$returnAddress"
    val gameRequest = GameRequest(user_id, access_token, returnAddress, crawlerAddress, Some(GameRequest.fbPostType))
//    dataRetriever ! RetrieveFBPosts(gameRequest)
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
