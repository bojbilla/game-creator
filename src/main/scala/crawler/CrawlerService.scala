package crawler


import akka.actor.{PoisonPill, ActorLogging, Actor, Props}
import crawler.CrawlerService.{FinishedCrawling, FetchData}
import crawler.common.{GraphResponses, FBSimpleParameters}
import crawler.common.GraphResponses.Page
import crawler.common.RetrieveEntitiesService.{FinishedRetrievingEntities, RetrieveEntities}
import crawler.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts}
import database.MongoDatabaseService
import database.MongoDatabaseService.{SaveFBPage, SaveFBTaggedPost}
import reactivemongo.api.DefaultDB
import server.domain.Domain.{TooManyRequests, Error, Done}
import server.domain.RestMessage
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpResponse, HttpRequest}
import scala.reflect.runtime.universe._
import reflect.ClassTag

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
      if (!currentlyCrawling.contains(userId)){
        val crawler = context.actorOf(CrawlerWorker.props(database))
        crawler ! FetchData(userId, accessToken)
        currentlyCrawling = currentlyCrawling + userId
        sender() ! Done("Fetching Data for " + userId)
      } else {
        sender ! TooManyRequests("Already crawling for user " + userId)
      }

    case FinishedCrawling(userId) =>
      log.info(s"Finished Crawling for user: $userId")
      sender ! PoisonPill
      currentlyCrawling = currentlyCrawling - userId

    case _ =>
      log.info("Crawler service Received unexpected message")

  }
}
