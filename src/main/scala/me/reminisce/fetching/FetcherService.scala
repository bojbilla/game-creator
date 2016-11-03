package me.reminisce.fetching

import java.util.concurrent.ConcurrentHashMap

import akka.actor._
import com.github.nscala_time.time.Imports._
import me.reminisce.analysis.DataAnalyser
import me.reminisce.analysis.DataAnalyser.FinalAnalysis
import me.reminisce.database.MongoDBEntities.LastFetched
import me.reminisce.database.MongoDatabaseService.SaveLastFetchedTime
import me.reminisce.database.{MongoCollections, MongoDatabaseService}
import me.reminisce.fetching.FetcherService._
import me.reminisce.server.domain.Domain.{AlreadyFresh, GraphAPIInvalidToken, GraphAPIUnreachable}
import me.reminisce.server.domain.RESTHandler.Confirmation
import me.reminisce.server.domain.{Domain, RestMessage}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument
import spray.client.pipelining._
import spray.http.StatusCodes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[me.reminisce.fetching.FetcherService]] and case classes for message passing
  */
object FetcherService {
  /**
    * The fetcher can not fetch for a user more than once concurrently
    */
  private val currentlyFetching = new ConcurrentHashMap[String, Boolean]()

  case class FetchData(userId: String, accessToken: String) extends RestMessage

  case class FetchDataSince(accessToken: String, since: DateTime) extends RestMessage

  case class FinishedFetching(userId: String)

  case class AckFetching(message: String)

  /**
    * Creates a fetcher service
    *
    * @param database the database in which the data will be stored
    * @return props for the created fetcher service
    */
  def props(database: DefaultDB): Props =
  Props(new FetcherService(database))
}

/**
  * Fetcher service actor, it is the main supervisor of the fetching process
  *
  * @param database the database to store the data in
  */
class FetcherService(database: DefaultDB) extends FBCommunicationManager {

  /**
    * Service entry point handles the following messages:
    * - FetchData(userId, accessToken): request for fetching data for user with ID userId and token accessToken
    * - FinishedFetching(userId): report from the worker that the fetching process is done, stores current time as
    * last fetched time
    *
    * @return Nothing
    */
  def receive = {
    case FetchData(userId, accessToken) =>
      val client = sender()
      if (!currentlyFetching.putIfAbsent(userId, true)) {
        val lastFetched = database[BSONCollection](MongoCollections.lastFetched)
        val query = BSONDocument(
          "userId" -> userId
        )
        val currentTime = DateTime.now
        lastFetched.find(query).cursor[LastFetched]().collect[List]().map {
          list => list.map(elm => elm.date).headOption
        }.onComplete {
          case Success(Some(time)) => conditionalFetch(currentTime, time, userId, accessToken, client)
          case _ => conditionalFetch(currentTime, new DateTime(1000), userId, accessToken, client)
        }
      } else {
        sender ! Domain.TooManyRequests("Already fetching for user " + userId)
      }
    case _ =>
      log.info("Fetcher service Received unexpected message")
  }

  /**
    * If the service starts fetching, we wait for the worker's feedback then acknowledge the end of the fetching to the
    * initial client
    *
    * @param client fetching requester
    * @return Nothing
    */
  def waitForFeedback(client: ActorRef): Receive = {
    case FinishedFetching(userId) =>
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveLastFetchedTime
      sender ! PoisonPill
      currentlyFetching.remove(userId)
      client ! Confirmation(s"Finished fetching for user: $userId")
      context.become(receive)
    case _ =>
      log.info("Fetcher service Received unexpected message")
  }

  /**
    * Determines if one has to fetch or not based on last time the data was fetched
    *
    * @param currentTime current time
    * @param lastFetched last time data was fetched
    * @return if fetching must be done or not
    */
  private def hasToFetch(currentTime: DateTime, lastFetched: DateTime): Boolean = {
    currentTime - 1.days > lastFetched
  }

  /**
    * Requests fetching to a worker if the last fetched was long enough in the past, if not sends an AlreadyFresh message
    * to the requester. If fetching does not occur, a summary update is still requested (some items might not have been
    * aggregated in summary yet).
    *
    * @param currentTime current time
    * @param lastFetched last time data was fetched
    * @param userId      user for which we want data
    * @param accessToken access token of the user
    * @param client      original fetch requester
    */
  private def conditionalFetch(currentTime: DateTime, lastFetched: DateTime, userId: String, accessToken: String,
                               client: ActorRef) = {
    if (hasToFetch(currentTime, lastFetched)) {
      val checkPath = s"$facebookPath/$userId?access_token=$accessToken"
      val validityCheck = pipelineRawJson(Get(checkPath))
      validityCheck.onComplete {
        case Success(response) =>
          response.status match {
            case OK =>
              val fetcher = context.actorOf(FetcherWorker.props(database, userId))
              fetcher ! FetchDataSince(accessToken, lastFetched)
              client ! AckFetching(s"Fetching Data for $userId")
              context.become(waitForFeedback(client))
            case _ =>
              log.error(s"Received a fetch request with an invalid token.")
              client ! GraphAPIInvalidToken(s"The specified token is invalid.")
              currentlyFetching.remove(userId)
          }
        case Failure(error) =>
          log.error(s"Facebook didn't respond \npath:$checkPath\n  ${error.toString}")
          client ! GraphAPIUnreachable(s"Could not reach Facebook graph API.")
          currentlyFetching.remove(userId)
        case any =>
          log.error(s"Facebook didn't respond \npath:$checkPath\n Unknown error: $any.")
          client ! GraphAPIUnreachable(s"Could not reach Facebook graph API.")
          currentlyFetching.remove(userId)
      }
    } else {
      client ! AlreadyFresh(s"Data for user $userId is fresh.")
      currentlyFetching.remove(userId)
      val dataAnalyser = context.actorOf(DataAnalyser.props(userId, database))
      dataAnalyser ! FinalAnalysis(Set(), Set())
      log.info(s"Requesting summary update for user $userId.")
    }
  }
}
