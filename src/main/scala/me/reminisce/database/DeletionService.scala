package me.reminisce.database

import akka.actor._
import me.reminisce.database.DeletionService.{ClearDatabase, RemoveExtraLikes, RemoveUser}
import me.reminisce.database.DeletionWorker.{DeleteSelectorMatch, DeletionResult, DropCollection}
import me.reminisce.server.domain.Domain.{ActionForbidden, Done, InternalError}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.MongoDBError
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}


object DeletionService {

  case class ClearDatabase(appMode: String) extends RestMessage

  case class RemoveUser(userId: String) extends RestMessage

  case class RemoveExtraLikes(userId: String, actualLikes: Set[String]) extends RestMessage

  def props(database: DefaultDB): Props =
    Props(new DeletionService(database))
}

class DeletionService(database: DefaultDB) extends Actor with ActorLogging {

  var workers = Set[ActorRef]()
  var results = Set[Boolean]()

  def receive = {
    case RemoveUser(userId) =>
      log.info(s"Removing user $userId from database.")
      val client = sender()
      removeUser(userId, client)
    case RemoveExtraLikes(userId, actualLikes) =>
      log.info(s"Cleaning likes for user $userId.")
      val client = sender()
      removeExtraLikes(userId, actualLikes, client)
    case ClearDatabase(appMode) =>
      if (appMode == "DEV") {
        log.info(s"Wiping database.")
        val client = sender()
        clearDatabase(client)
      } else {
        sender() ! ActionForbidden("The app is not in development mode.")
      }
    case _ =>
      log.error("Deletion service received an unexpected message.")
  }

  def awaitFeedBack(client: ActorRef): Receive = {
    case MongoDBError(m) =>
      workers.foreach(w => w ! PoisonPill)
      client ! InternalError(m)
    case DeletionResult(lastError) =>
      workers -= sender()
      results += lastError
      verifyDone(client)
  }

  def removeUser(userId: String, client: ActorRef) = {
    val selector = BSONDocument("userId" -> userId)
    foreachCollection(client) {
      collectionName =>
        val collection = database[BSONCollection](collectionName)
        val worker = context.actorOf(DeletionWorker.props(collection))
        worker ! DeleteSelectorMatch(selector)
        workers += worker
    }
  }

  def removeExtraLikes(userId: String, actualLikes: Set[String], client: ActorRef): Unit = {
    val pageLikeCollection = database[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
    val selector = BSONDocument("userId" -> userId,
      "pageId" -> BSONDocument("$nin" -> actualLikes.toList)
    )
    context.become(awaitFeedBack(client))
    val worker = context.actorOf(DeletionWorker.props(pageLikeCollection))
    worker ! DeleteSelectorMatch(selector)
    workers += worker
  }

  def clearDatabase(client: ActorRef): Unit = {
    foreachCollection(client) {
      collectionName =>
        val collection = database[BSONCollection](collectionName)
        val worker = context.actorOf(DeletionWorker.props(collection))
        worker ! DropCollection()
        workers += worker
    }
  }

  def verifyDone(client: ActorRef) = {
    if (workers.isEmpty) {
      val errors = results.filter(!_)
      if (errors.isEmpty) {
        client ! Done("Deletion performed without error.")
      } else {
        client ! InternalError(s"Deletion finished with errors.")
      }
    }
  }

  def onCollections(client: ActorRef)(handle: List[String] => Unit)(default: () => Unit): Unit = {
    import scala.concurrent.ExecutionContext.Implicits.global
    database.collectionNames.onComplete {
      case Success(collectionNames) =>
        val filteredCollectionNames = collectionNames.filter(!_.startsWith("system."))
        if (filteredCollectionNames.size > 0) {
          handle(filteredCollectionNames)
        } else {
          default()
        }
      case Failure(e) =>
        client ! InternalError(s"Database error : $e.")
    }
  }

  def foreachCollection(client: ActorRef)(handle: String => Unit): Unit = {
    onCollections(client) {
      collectionNames =>
        context.become(awaitFeedBack(client))
        collectionNames.foreach(handle)
    } {
      () => client ! Done("Deletion performed without error.")
    }
  }

}

