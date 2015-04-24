package me.reminisce.fetcher

import akka.actor._
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.MongoDatabaseService.SaveLastFetchedTime
import me.reminisce.fetcher.FetcherService.{FetchData, FetchDataSince, FinishedFetching}
import me.reminisce.fetcher.common.FBCommunicationManager
import me.reminisce.mongodb.MongoDBEntities.LastFetched
import me.reminisce.server.domain.Domain.{AlreadyFresh, Done, GraphAPIInvalidToken, GraphAPIUnreachable}
import me.reminisce.server.domain.{Domain, RestMessage}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import spray.client.pipelining._
import spray.http.StatusCodes._

import scala.util.{Failure, Success}

/**
 * Created by roger on 05/03/15.
 */

object FetcherService {

  case class FetchData(user_id: String, access_token: String) extends RestMessage

  case class FetchDataSince(user_id: String, access_token: String, since: DateTime) extends RestMessage

  case class FinishedFetching(user_id: String)

  def props(database: DefaultDB): Props =
    Props(new FetcherService(database))
}

class FetcherService(database: DefaultDB) extends FBCommunicationManager {
  var currentlyFetching: Set[String] = Set()

  def receive() = {
    case FetchData(userId, accessToken) =>
      val client = sender()
      if (!currentlyFetching.contains(userId)) {
        val lastFetched = database[BSONCollection](MongoDatabaseService.lastFetchedCollection)
        val query = BSONDocument(
          "user_id" -> userId
        )
        val currentTime = DateTime.now
        lastFetched.find(query).cursor[LastFetched].collect[List]().map {
          list => list.map(elm => elm.date).head
        }.onComplete {
          case Success(time) => conditionalFetch(currentTime, time, userId, accessToken, client)
          case Failure(e) => conditionalFetch(currentTime, new DateTime(1000), userId, accessToken, client)
          case _ => conditionalFetch(currentTime, new DateTime(1000), userId, accessToken, client)
        }

      } else {
        sender ! Domain.TooManyRequests("Already fetching for user " + userId)
      }

    case FinishedFetching(userId) =>
      log.info(s"Finished fetching for user: $userId")
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveLastFetchedTime
      sender ! PoisonPill
      currentlyFetching = currentlyFetching - userId

    case _ =>
      log.info("Fetcher service Received unexpected message")

  }

  def hasToFetch(currentTime: DateTime, lastFetched: DateTime): Boolean = {
    currentTime - 10.seconds > lastFetched
  }

  def conditionalFetch(currentTime: DateTime, lastFetched: DateTime, userId: String, accessToken: String,
                       client: ActorRef) = {
    if (hasToFetch(currentTime, lastFetched)) {
      val checkPath = s"$facebookPath/$userId?access_token=$accessToken"
      val validityCheck = pipelineRawJson(Get(checkPath))
      validityCheck.onComplete {
        case Success(response) =>
          response.status match {
            case OK =>
              val fetcher = context.actorOf(FetcherWorker.props(database))
              fetcher ! FetchDataSince(userId, accessToken, lastFetched)
              currentlyFetching = currentlyFetching + userId
              client ! Done(s"Fetching Data for $userId")
            case _ =>
              log.error(s"Received a fetch request with an invalid token.")
              client ! GraphAPIInvalidToken(s"The specified token is invalid.")
          }
        case Failure(error) =>
          log.error(s"Facebook didn't respond \npath:$checkPath\n  ${error.toString}")
          client ! GraphAPIUnreachable(s"Could not reach Facebook graph API.")
      }
    } else {
      client ! AlreadyFresh(s"Data for user $userId is fresh.")
    }
  }
}
