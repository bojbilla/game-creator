package service

import akka.actor.SupervisorStrategy.Stop
import akka.actor._
import entities.Entities._
import mongodb.MongoDBEntities.{FBPageLike, FBPage}
import reactivemongo.api.{QueryOpts, DefaultDB}
import server.domain.RestMessage
import service.GameGenerator.{CreateBoard}
import service.RandomDocumentGetter.{GetDocument, RetrievedDocument}
import service.tile_generator.TileGenerator
import service.tile_generator.TileGenerator.{CreateTimelineTile, FailedTileCreation, FinishedTileCreation, CreateMultipleChoiceTile}

import scala.concurrent.{ExecutionContext, Promise, Future, ExecutionContextExecutor}
import scala.util.{Failure, Success, Random}

/**
 * Created by roger on 17/11/14.
 */

object GameGenerator {

  def props(database: DefaultDB): Props =
    Props(new GameGenerator(database))

  case class CreateBoard(user_id: String) extends RestMessage
  case class FinishedBoardCreation(board: Board)
}

class GameGenerator(database: DefaultDB) extends Actor with ActorLogging{
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  var tiles: List[Tile] = List()

  def receive = {
    case CreateBoard(user_id) =>
      val client = sender()
      val tileActors = (0 to 8).map( _ => context.actorOf(TileGenerator.props(database)))
      tileActors.foreach{ a =>
        Random.nextInt(4) match {
          case 0 => a ! CreateTimelineTile(user_id)
          case 1 | 2 | 3 => a ! CreateMultipleChoiceTile(user_id)

        }
      }
      context.become(awaitTiles(client, tileActors))
    case x => log.error("GameGenerator received unexpected Message " + x)
  }

  def awaitTiles(client: ActorRef, workers: IndexedSeq[ActorRef]): Receive = {
    case FinishedTileCreation(user_id, tile) =>
      tiles = tile :: tiles
//      sender() ! PoisonPill
      log.info(s"Created ${tiles.length} questions")
      if (tiles.length == 9){
        val board = Board(user_id, tiles)
        client ! board
      }
    case FailedTileCreation(message) =>
      log.error(s"Failed board creation: $message")
      workers.foreach(w => w ! PoisonPill)
      client ! server.domain.Domain.Error(message)
  }

}
