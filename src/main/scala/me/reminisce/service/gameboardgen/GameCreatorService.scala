package me.reminisce.service.gameboardgen

import akka.actor._
import me.reminisce.fetcher.FetcherService
import me.reminisce.fetcher.FetcherService.FetchData
import me.reminisce.server.domain.{RESTHandlerCreator, RestMessage}
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
          (user_id: String, access_token: String) =>
            fetchData {
              FetchData(user_id, access_token)
            }
        }
      }
    } ~ path("gameboard") {
      parameters("user_id", "access_token", "strategy" ? "random") {
        (user_id: String, access_token: String, strategy: String) =>
          createBoard(CreateBoard(access_token, strategy), user_id)
      }
    }
  }


  def createBoard(message: RestMessage, user_id: String): Route = {
    log.info("Creating game board")
    val generator = context.actorOf(GameGenerator.props(db, user_id))
    ctx => perRequest(ctx, generator, message)
  }

  def fetchData(message: RestMessage): Route = {
    val fetcherService = context.actorOf(FetcherService.props(db))
    ctx => perRequest(ctx, fetcherService, message)
  }

}
