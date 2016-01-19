package me.reminisce.database

import akka.actor._
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveExtraLikes, RemoveUser}
import me.reminisce.database.DeletionWorker.{DeleteSelectorMatch, DeletionResult, DropCollection}
import me.reminisce.gameboard.questions.QuestionGenerator.MongoDBError
import me.reminisce.server.ApplicationConfiguration
import me.reminisce.server.domain.Domain.{ActionForbidden, Done, InternalError}
import me.reminisce.server.domain.RestMessage
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[me.reminisce.database.DeletionService]] and case class definitions for message passing.
  */
object DeletionService {

  case class ClearDatabase() extends RestMessage

  case class RemoveUser(userId: String) extends RestMessage

  case class RemoveExtraLikes(userId: String, actualLikes: Set[String]) extends RestMessage

  /**
    * Creates a deletion service actors
    * @param database the database on which the service must operate
    * @return props for the generated deletion service
    */
  def props(database: DefaultDB): Props =
    Props(new DeletionService(database))
}

/**
  * A deletion service actor which can delete parts of the data in a database
  * @constructor create a deletion service operating on a database
  * @param database a reference to the database on which the service operates
  */
class DeletionService(database: DefaultDB) extends Actor with ActorLogging {

  /**
    * Entry point of the service. Handles the following messages :
    * - RemoveUser("userId") to remove all data from user "userId"
    * - RemoveExtraLikes("userId", actualLikes) to remove the pages not liked anymore based on the actualLikes list
    * - ClearDatabase() to clear the whole database, only possible if
    * [[me.reminisce.server.ApplicationConfiguration.appMode]] is "DEV"
    *
    * Creates a worker and then waits on feedback.
    * @return Nothing
    */
  def receive = {
    case RemoveUser(userId) =>
      log.info(s"Removing user $userId from database.")
      val client = sender()
      removeUser(userId, client)
    case RemoveExtraLikes(userId, actualLikes) =>
      log.info(s"Cleaning likes for user $userId.")
      val client = sender()
      removeExtraLikes(userId, actualLikes, client)
    case ClearDatabase() =>
      if (ApplicationConfiguration.appMode == "DEV") {
        log.info(s"Wiping database.")
        val client = sender()
        clearDatabase(client)
      } else {
        sender() ! ActionForbidden("The app is not in development mode.")
      }
    case any =>
      log.error(s"Deletion service received an unexpected message : $any.")
      sender() ! ActionForbidden(s"Deletion service received an unexpected message : $any.")
  }

  /**
    * Waiting on feedback from the workers until there is no worker left
    * @param client the original deletion requester
    * @param workers the currently working workers
    * @param results an aggregation of the deletion results sent by the workers
    * @return Nothing
    */
  private def awaitFeedBack(client: ActorRef, workers: Set[ActorRef], results: Set[Boolean]): Receive = {
    case MongoDBError(m) =>
      workers.foreach(w => w ! PoisonPill)
      client ! InternalError(m)
    case DeletionResult(lastError) =>
      verifyDone(client, workers - sender(), results + lastError)
    case any =>
      log.error(s"Deletion service received an unexpected message : $any.")
  }

  /**
    * Creates workers and requests the deletion of every piece of data from that user.
    * @param userId userId of the user to delete
    * @param client the original requester for deletion
    */
  private def removeUser(userId: String, client: ActorRef) = {
    val selector = BSONDocument("userId" -> userId)
    foreachCollection(client) {
      worker =>
        worker ! DeleteSelectorMatch(selector)
    }
  }

  /**
    * Creates workers and request them to delete the extra likes for a particular user
    * @param userId the handled user
    * @param actualLikes the actual likes of the user
    * @param client the original deletion requester
    */
  private def removeExtraLikes(userId: String, actualLikes: Set[String], client: ActorRef): Unit = {
    val pageLikeCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    val selector = BSONDocument("userId" -> userId,
      "pageId" -> BSONDocument("$nin" -> actualLikes.toList)
    )
    val worker = context.actorOf(DeletionWorker.props(pageLikeCollection))
    context.become(awaitFeedBack(client, Set(worker), Set()))
    worker ! DeleteSelectorMatch(selector)
  }

  /**
    * Create workers to delete each collection
    * @param client the original deletion requester
    */
  private def clearDatabase(client: ActorRef): Unit = {
    foreachCollection(client) {
      worker =>
        worker ! DropCollection()
    }
  }

  /**
    * Verifies if the work is done , if so it reports back to client.
    * @param client original deletion requester
    * @param workers currently working workers
    * @param results aggregated deletion results
    */
  private def verifyDone(client: ActorRef, workers: Set[ActorRef], results: Set[Boolean]) = {
    if (workers.isEmpty) {
      val errors = results.filter(!_)
      if (errors.isEmpty) {
        client ! Done("Deletion performed without error.")
      } else {
        client ! InternalError(s"Deletion finished with errors.")
      }
    }
    context.become(awaitFeedBack(client, workers, results))
  }

  /**
    * Performs operation defined by handle on every collection if there is at least one, otherwise calls default
    * @param client original requester
    * @param handle performs operations on a list of collection names
    * @param default default operation if there is no collection
    */
  private def onCollections(client: ActorRef)(handle: List[String] => Unit)(default: () => Unit): Unit = {
    database.collectionNames.onComplete {
      case Success(collectionNames) =>
        val filteredCollectionNames = collectionNames.filter(!_.startsWith("system."))
        if (filteredCollectionNames.nonEmpty) {
          handle(filteredCollectionNames)
        } else {
          default()
        }
      case Failure(e) =>
        client ! InternalError(s"Database error : $e.")
      case any =>
        client ! InternalError(s"Unknown error: $any.")
    }
  }

  /**
    * Calls the handleWithWorkers routine on a worker for each collection found
    * @param client original requester
    * @param handleWithWorkers operation to perform on a worker constructed for a particular collection
    */
  private def foreachCollection(client: ActorRef)(handleWithWorkers: ActorRef => Unit): Unit = {
    onCollections(client) {
      collectionNames =>
        val workers = collectionNames.map {
          collectionName =>
            val collection = database[BSONCollection](collectionName)
            context.actorOf(DeletionWorker.props(collection))
        }
        context.become(awaitFeedBack(client, workers.toSet, Set()))
        workers.foreach(handleWithWorkers)
    } {
      () => client ! Done("Deletion performed without error.")
    }
  }

}

