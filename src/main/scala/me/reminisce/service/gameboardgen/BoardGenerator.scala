package me.reminisce.service.gameboardgen

import akka.actor.{Actor, ActorContext, ActorLogging, ActorRef}
import me.reminisce.service.gameboardgen.BoardGenerator.FailedBoardGeneration
import me.reminisce.service.gameboardgen.GameGenerator.InitBoardCreation
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}

import scala.concurrent.ExecutionContextExecutor
import scala.util.{Failure, Success}

object BoardGenerator {

  case class FailedBoardGeneration(message: String)

}

abstract class BoardGenerator(database: DefaultDB, user_id: String) extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  def createGame(client: ActorRef): Unit

  def receive = {
    case InitBoardCreation() =>
      val client = sender()
      createGame(client)
    case any =>
      sender ! FailedBoardGeneration(s"Received any : $any")
      log.error("Received any : " + any)
  }

  def findOne[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (Option[T] => Unit))
                (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).one[T].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"Could not generate board for user $user_id. Cause : $e")
    }
  }

  def findSome[T](collection: BSONCollection, selector: BSONDocument, client: ActorRef)(f: (List[T] => Unit))
                 (implicit reader: BSONDocumentReader[T]): Unit = {
    collection.find(selector).cursor[T].collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! FailedBoardGeneration(s"Could not generate board for user $user_id. Cause : $e")
    }
  }
}
