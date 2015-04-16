package me.reminisce.mock

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.entities.Entities.{Board, Tile, TileQuestionType}
import me.reminisce.mock.MockBoardService.GetGameBoard
import me.reminisce.mock.data.{GeolocationMock, MultipleChoiceMock, TimelineMock}
import me.reminisce.server.domain.RestMessage
import org.json4s.DefaultFormats
import spray.httpx.Json4sSupport

import scala.concurrent.ExecutionContextExecutor

object MockBoardService {

  case class GetGameBoard(user_id: String, token: String) extends RestMessage

}

/**
 * Created by roger on 10/11/14.
 */
class MockBoardService extends Actor with ActorLogging with Json4sSupport {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  val json4sFormats = DefaultFormats

  def receive = {
    case GetGameBoard(user_id, token) =>
      val mc = new MultipleChoiceMock()
      val tl = new TimelineMock()
      val gl = new GeolocationMock()
      val tile1 = Tile(TileQuestionType.MultipleChoice, mc.multiQuestion1, mc.multiQuestion2, mc.multiQuestion3)
      val tile2 = Tile(TileQuestionType.Timeline, tl.timeline1, tl.timeline2, tl.timeline3)
      val tile3 = Tile(TileQuestionType.Geolocation, gl.geolocation1, gl.geolocation2, gl.geolocation3)
      val board = Board(mc.multiQuestion1.user_id, List(tile1, tile2, tile3))
      sender() ! board
    case _ => log.error("MockBoardService received strange message")
  }
}
