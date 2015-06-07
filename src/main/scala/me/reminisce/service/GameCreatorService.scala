package me.reminisce.service

import akka.actor._
import me.reminisce.database.DeletionService
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveUser}
import me.reminisce.fetcher.FetcherService
import me.reminisce.fetcher.FetcherService.FetchData
import me.reminisce.server.domain.{RESTHandlerCreator, RestMessage}
import me.reminisce.service.gameboardgen.GameGenerator
import me.reminisce.service.gameboardgen.GameGenerator.CreateBoard
import reactivemongo.api.DefaultDB
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse}
import spray.httpx.Json4sSupport
import spray.routing._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

object GameCreatorService {


}

trait GameCreatorServiceActor extends GameCreatorService {
  def actorRefFactory = context

  def receive = runRoute(gameCreatorRoutes)
}

trait GameCreatorService extends HttpService with RESTHandlerCreator with Actor with ActorLogging with Json4sSupport {
  def actorRefFactory: ActorContext

  val db: DefaultDB

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )
  val gameCreatorRoutes = {

    path("fetchData") {
      get {
        parameters('user_id.as[String], 'access_token.as[String]) {
          (userId: String, accessToken: String) =>
            fetchData {
              FetchData(userId, accessToken)
            }
        }
      }
    } ~ path("gameboard") {
      get {
        parameters("user_id", "access_token", "strategy" ? "choose") {
          (userId: String, accessToken: String, strategy: String) =>
            createBoard(CreateBoard(accessToken, strategy), userId)
        }
      }
    } ~ path("removeUser") {
      delete {
        parameters("user_id") {
          (userId: String) =>
            removeUser(RemoveUser(userId), userId)
        }
      }
    } ~ path("dropDatabase") {
      delete {
        parameters("UNUSED" ? "") {
          //ugly fix
          (UNUSED: String) =>
            dropDatabase(ClearDatabase(ApplicationConfiguration.appMode))
        }
      }
    }
  }


  def createBoard(message: RestMessage, userId: String): Route = {
    log.info("Creating game board.")
    val generator = context.actorOf(GameGenerator.props(db, userId))
    ctx => perRequest(ctx, generator, message)
  }

  def fetchData(message: RestMessage): Route = {
    val fetcherService = context.actorOf(FetcherService.props(db))
    ctx => perRequest(ctx, fetcherService, message)
  }

  def removeUser(message: RestMessage, userId: String): Route = {
    val deletionService = context.actorOf(DeletionService.props(db))
    ctx => perRequest(ctx, deletionService, message)
  }

  def dropDatabase(message: RestMessage): Route = {
    val deletionService = context.actorOf(DeletionService.props(db))
    ctx => perRequest(ctx, deletionService, message)
  }

}
