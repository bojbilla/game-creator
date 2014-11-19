package service

import akka.actor._
import akka.event.Logging
import data_gathering.DataRetriever
import data_gathering.DataRetriever.RetrieveFBPages
import database.MongoDatabaseService
import database.MongoDatabaseService.SaveFBPage
import messages.GraphResponses
import routing.{PerRequest, PerRequestCreator}
import server.domain.RestMessage
import service.GameCreatorService.CrawlerTest
import mongodb.MongoDBEntities.{FBTag, FBPhoto, FBPage}
import mongodb.{Chicken, Possibility}
import reactivemongo.api.indexes.{Index, CollectionIndexesManager}
import reactivemongo.api.{MongoConnection, MongoDriver, DefaultDB}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import service.GameGenerator.{WhichPageDidYouLike, CreatedWhichPageDidYouLike}
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}
import spray.httpx.Json4sSupport
import spray.routing._
import server.Server
import spray.http.StatusCode
import spray.http.StatusCodes._
//import spray.routing.RejectionHandler.Default
import scala.concurrent.ExecutionContext.Implicits.global

import akka.event.Logging._
import spray.util.LoggingContext
import spray.http._
import akka.event.LoggingAdapter

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
  val crawlerHost: String
  val dataRetriever: ActorRef

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )
  val gameCreatorRoutes = {

    pathEndOrSingleSlash {
      get {
        complete {
          val token = "CAAJMWTZBZC0vQBAMVVHQWJGDFEZANCMJRZBuY5VDSwG4Ctu1fVZB93CP72ZAHCzMtQg0qriUHDjtDRdtSI0agHDD84YVzlkSu004gUq75c10CvwlWRBFZAsURLHyUbnyU5WPPra6cMw03sQB3DblYEZABegqGr06EyZB1xWBTmDfwZB8QtbeJXhPAp76OrcmXLeYkZD"
          val user_id = "21341"
          val return_address = s"http://${Server.hostName}:${Server.port}/liked_pages/$user_id"
          val crawler = s"http://localhost:9000/liked_pages?user_id=$user_id&access_token=$token&return_address=$return_address"

          val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
          val responseF = pipeline(Get(crawler))
          responseF.onComplete(r => log.info("we received " + r.get.status))

          "we are starting i think"
        }
      }
    } ~ path ("gameboard") {
        get {
          parameters('user_id.as[String], 'access_token.as[String])
          { (user_id: String, access_token: String) =>
            complete{
              val return_address = s"http://${Server.hostName}:${Server.port}/liked_pages/$user_id"
              val crawlerAddress = s"$crawlerHost/liked_pages?user_id=$user_id&access_token=$access_token&return_address=$return_address"

              val gameRequest = GameRequest(user_id, access_token, return_address, crawlerAddress, Some(GameRequest.fbPageType))
              dataRetriever ! RetrieveFBPages(gameRequest)
              OK
            }
          }
        }
    } ~ path("liked_pages" / Segment) { user_id =>
      post {
        {
          entity(as[List[GraphResponses.Page]]) { pages =>
            complete{

//              val fbPages = pages.map { p =>
//                val photo = p.photos.flatMap(photoRoot => photoRoot.data.map(photo => photo))
//                val fbPhoto = photo.map{photo =>
//                  val tags = photo.tags.flatMap(tagRoot => tagRoot.data).map {
//                    tags => tags.map { tag => {
//                      FBTag(tag.id, tag.name, tag.created_time, tag.x, tag.y)
//                    }
//                    }
//                  }
//
//                  FBPhoto(photo.id, photo.source, photo.created_time, tags)
//                }
//                FBPage(None, Some(user_id), p.id, p.name, fbPhoto)
//              }
//
//              saveToDB(fbPages)
//
//              log.info(s"we received for $user_id ")
//              println("wuff ")
              val mongoSaver = context.actorOf(MongoDatabaseService.props(user_id, db))
              mongoSaver ! SaveFBPage(pages)
              OK
            }
          }
        }
      }
    } ~ path("test") {
      parameters('user_id.as[String]) { user_id: String =>
        tryToCreateBoard{
          WhichPageDidYouLike(user_id)
        }

      }
    }
  }

  def tryToCreateBoard(message: RestMessage): Route = {
    val generator = context.actorOf(GameGenerator.props(db))
    ctx => perRequest(ctx, generator, message)
  }
}
