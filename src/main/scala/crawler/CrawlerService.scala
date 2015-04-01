package crawler

import akka.actor._
import crawler.CrawlerService.{FinishedCrawling, FetchData}
import crawler.FacebookConfig.FacebookServiceConfig
import crawler.common.{GraphResponses, FBSimpleParameters}
import com.github.nscala_time.time.Imports._
import crawler.common.GraphResponses.Page
import crawler.common.RetrieveEntitiesService.{FinishedRetrievingEntities, RetrieveEntities}
import crawler.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts}
import database.MongoDatabaseService
import database.MongoDatabaseService.{SaveLastCrawledTime, SaveFBPage, SaveFBTaggedPost}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import server.domain.Domain._
import server.domain.{Domain, RestMessage}
import spray.client.pipelining._
import spray.http.StatusCodes._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}
import scala.reflect.runtime.universe._
import reflect.ClassTag
import mongodb.MongoDBEntities.LastCrawled
import scala.util.{Failure, Success}

import scala.concurrent.Future

/**
 * Created by roger on 05/03/15.
 */

object CrawlerService {

  case class FetchData(user_id: String, access_token: String) extends RestMessage
  case class FinishedCrawling(user_id: String)

  def props(database: DefaultDB): Props =
    Props(new CrawlerService(database))
}
class CrawlerService(database: DefaultDB) extends Actor with ActorLogging{
  var currentlyCrawling: Set[String] = Set()

  def receive() = {
    case FetchData(userId, accessToken) =>
      val client = sender()
      if (!currentlyCrawling.contains(userId)){
        import scala.concurrent.ExecutionContext.Implicits.global
        val lastCrawled = database[BSONCollection](MongoDatabaseService.lastCrawledCollection)
        val query = BSONDocument(
          "user_id" -> userId
        )
        val curTime = DateTime.now
        lastCrawled.find(query).cursor[LastCrawled].collect[List]().map {
          list => list.map(elm => elm.date).head
        }.onComplete {
          case Success(time) => conditionalCrawl(curTime, time, userId, accessToken, client)
          case Failure(e) => conditionalCrawl(curTime, new DateTime(0), userId, accessToken, client)
          case _ => conditionalCrawl(curTime, new DateTime(0), userId, accessToken, client)
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

  def hasToCrawl(curTime : DateTime, time : DateTime): Boolean = {
    curTime - 10.seconds > time
  }

  def conditionalCrawl(curTime : DateTime, time : DateTime, userId : String, accessToken : String, client: ActorRef) = {
    if (hasToCrawl(curTime, time)) {
      val graphAPIPath = FacebookServiceConfig.facebookHostAddress
      val checkPath = s"$graphAPIPath/v2.2/$userId?access_token=$accessToken"
      implicit def dispatcher = context.dispatcher
      val validityCheck = Get(checkPath) ~> sendReceive
      validityCheck.onComplete {
        case Success(response) =>
          response.status match {
            case OK =>
              val crawler = context.actorOf(CrawlerWorker.props(database))
              crawler ! FetchData(userId, accessToken)
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
