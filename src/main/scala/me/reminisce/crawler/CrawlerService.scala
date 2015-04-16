package me.reminisce.crawler

import akka.actor._
import com.github.nscala_time.time.Imports._
import me.reminisce.crawler.CrawlerService.{FetchData, FetchDataSince, FinishedCrawling}
import me.reminisce.crawler.common.FBCommunicationManager
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.MongoDatabaseService.SaveLastCrawledTime
import me.reminisce.mongodb.MongoDBEntities.LastCrawled
import me.reminisce.server.domain.Domain.{AlreadyFresh, GraphAPIUnreachable, GraphAPIInvalidToken, Done}
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

object CrawlerService {

  case class FetchData(user_id: String, access_token: String) extends RestMessage

  case class FetchDataSince(user_id: String, access_token: String, since: DateTime) extends RestMessage

  case class FinishedCrawling(user_id: String)

  def props(database: DefaultDB): Props =
    Props(new CrawlerService(database))
}

class CrawlerService(database: DefaultDB) extends FBCommunicationManager {
  var currentlyCrawling: Set[String] = Set()

  def receive() = {
    case FetchData(userId, accessToken) =>
      val client = sender()
      if (!currentlyCrawling.contains(userId)) {
        val lastCrawled = database[BSONCollection](MongoDatabaseService.lastCrawledCollection)
        val query = BSONDocument(
          "user_id" -> userId
        )
        val currentTime = DateTime.now
        lastCrawled.find(query).cursor[LastCrawled].collect[List]().map {
          list => list.map(elm => elm.date).head
        }.onComplete {
          case Success(time) => conditionalCrawl(currentTime, time, userId, accessToken, client)
          case Failure(e) => conditionalCrawl(currentTime, new DateTime(1000), userId, accessToken, client)
          case _ => conditionalCrawl(currentTime, new DateTime(1000), userId, accessToken, client)
        }

      } else {
        sender ! Domain.TooManyRequests("Already crawling for user " + userId)
      }

    case FinishedCrawling(userId) =>
      log.info(s"Finished Crawling for user: $userId")
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveLastCrawledTime
      sender ! PoisonPill
      currentlyCrawling = currentlyCrawling - userId

    case _ =>
      log.info("Crawler service Received unexpected message")

  }

  def hasToCrawl(currentTime: DateTime, lastCrawled: DateTime): Boolean = {
    currentTime - 10.seconds > lastCrawled
  }

  def conditionalCrawl(currentTime: DateTime, lastCrawled: DateTime, userId: String, accessToken: String,
                       client: ActorRef) = {
    if (hasToCrawl(currentTime, lastCrawled)) {
      val checkPath = s"$facebookPath/$userId?access_token=$accessToken"
      val validityCheck = pipelineRawJson(Get(checkPath))
      validityCheck.onComplete {
        case Success(response) =>
          response.status match {
            case OK =>
              val crawler = context.actorOf(CrawlerWorker.props(database))
              crawler ! FetchDataSince(userId, accessToken, lastCrawled)
              currentlyCrawling = currentlyCrawling + userId
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
