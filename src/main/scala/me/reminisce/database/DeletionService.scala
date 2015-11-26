package me.reminisce.database

import akka.actor._
import me.reminisce.ApplicationConfiguration
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveExtraLikes, RemoveUser}
import me.reminisce.database.DeletionWorker.{DeleteSelectorMatch, DeletionResult, DropCollection}
import me.reminisce.server.domain.Domain.{ActionForbidden, Done, InternalError}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.MongoDBError
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object DeletionService {

  case class ClearDatabase() extends RestMessage

  case class RemoveUser(userId: String) extends RestMessage

  case class RemoveExtraLikes(userId: String, actualLikes: Set[String]) extends RestMessage

  def props(database: DefaultDB): Props =
    Props(new DeletionService(database))
}

class DeletionService(database: DefaultDB) extends Actor with ActorLogging {

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

  def awaitFeedBack(client: ActorRef, workers: Set[ActorRef], results: Set[Boolean]): Receive = {
    case MongoDBError(m) =>
      workers.foreach(w => w ! PoisonPill)
      client ! InternalError(m)
    case DeletionResult(lastError) =>
      verifyDone(client, workers - sender(), results + lastError)
    case any =>
      log.error(s"Deletion service received an unexpected message : $any.")
  }

  def removeUser(userId: String, client: ActorRef) = {
    val selector = BSONDocument("userId" -> userId)
    foreachCollection(client) {
      worker =>
        worker ! DeleteSelectorMatch(selector)
    }
  }

  def removeExtraLikes(userId: String, actualLikes: Set[String], client: ActorRef): Unit = {
    val pageLikeCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    val selector = BSONDocument("userId" -> userId,
      "pageId" -> BSONDocument("$nin" -> actualLikes.toList)
    )
    val worker = context.actorOf(DeletionWorker.props(pageLikeCollection))
    context.become(awaitFeedBack(client, Set(worker), Set()))
    worker ! DeleteSelectorMatch(selector)
  }

  def clearDatabase(client: ActorRef): Unit = {
    foreachCollection(client) {
      worker =>
        worker ! DropCollection()
    }
  }

  def verifyDone(client: ActorRef, workers: Set[ActorRef], results: Set[Boolean]) = {
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

  def onCollections(client: ActorRef)(handle: List[String] => Unit)(default: () => Unit): Unit = {
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
    }
  }

  def foreachCollection(client: ActorRef)(handleWithWorkers: ActorRef => Unit): Unit = {
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

