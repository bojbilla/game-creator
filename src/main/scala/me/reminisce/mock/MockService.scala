package me.reminisce.mock

import akka.actor.{Actor, ActorContext, Props}
import me.reminisce.entities.Entities.Board
import me.reminisce.mock.MockBoardService.GetGameBoard
import me.reminisce.server.domain.RestMessage
import me.reminisce.server.domain.resthandling.RESTHandlerCreator
import spray.routing._

/**
 * Created by roger on 10/11/14.
 */

object MockService {

  case class BoardComplete(board: Board)

}

trait MockServiceActor extends Actor with MockService {

  def actorRefFactory = context

  def receive = runRoute(mockRoutes)

}

trait MockService extends HttpService with RESTHandlerCreator with Actor {
  def actorRefFactory: ActorContext

  val mockRoutes = {
    get {
      pathEndOrSingleSlash {
        complete("Say hello")
      }
      pathPrefix("mock") {
        path("board") {
          parameters('user_id.as[String], 'token.as[String]) { (user_id, token) =>
            getMockBoard(GetGameBoard(user_id, token))
          }
        }
      }
    }
  }

  def getMockBoard(message: RestMessage): Route = {
    val mocker = actorRefFactory.actorOf(Props[MockBoardService])
    ctx => perRequest(ctx, mocker, message)
  }
}