package me.reminisce.database

import akka.actor.{Actor, ActorLogging, ActorRef, Props}
import me.reminisce.database.DeletionWorker.{DeleteSelectorMatch, DeletionResult, DropCollection}
import me.reminisce.gameboard.questions.QuestionGenerator.MongoDBError
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[me.reminisce.database.DeletionWorker]] and case class definitions for message passing
  */
object DeletionWorker {

  case class DeleteSelectorMatch(selector: BSONDocument)

  case class DropCollection()

  case class DeletionResult(ok: Boolean)

  /**
    * Creates a worker which operates on a particular collection
    * @param collection collection to perform actions on
    * @return props for the created worker
    */
  def props(collection: BSONCollection): Props =
    Props(new DeletionWorker(collection))
}

/**
  * A deletion worker which can delete data on a particular collection
  * @constructor create a worker to operate on a collection
  * @param collection collection to operate on
  */
class DeletionWorker(collection: BSONCollection) extends Actor with ActorLogging {

  /**
    * Message handling, handles the following messages:
    * - DeleteSelectorMatch(selector): deletes everything matching selector
    * - DropCollection(): drops the collection
    * @return Nothing
    */
  def receive = {
    case DeleteSelectorMatch(selector) =>
      val client = sender()
      delete(selector, client)
    case DropCollection() =>
      val client = sender()
      dropCollection(client)
    case _ =>
      log.error("Deleter service received an unexpected message.")
  }

  /**
    * Deletes everything that matches selector
    * @param selector the selector to be matched
    * @param client the service requesting deletion
    */
  private def delete(selector: BSONDocument, client: ActorRef) = {
    collection.remove(selector).onComplete {
      case Success(lastError) =>
        client ! DeletionResult(ok = lastError.ok)
        if (!lastError.ok)
          log.error(s"Error while deleting : ${lastError.getMessage()}")
      case Failure(e) =>
        client ! MongoDBError(s"Database error in deletion worker : $e --- ${collection.name}")
      case any =>
        client ! MongoDBError(s"Unknown database error: $any.")
    }
  }

  /**
    * Drops the collection
    * @param client the service requesting the deletion
    */
  private def dropCollection(client: ActorRef) = {
    collection.drop().onComplete {
      case Success(e) =>
        client ! DeletionResult(ok = true)
      case Failure(e) =>
        log.error(s"Error while dropping ${collection.name} : $e")
        client ! DeletionResult(ok = false)
      case any =>
        log.error(s"Unknown error while dropping ${collection.name} : $any")
        client ! DeletionResult(ok = false)
    }
  }

}