package crawler

import akka.actor._
import crawler.CrawlerService.{FetchDataSince, FinishedCrawling}
import crawler.common.FBSimpleParameters
import crawler.common.RetrieveEntitiesService.RetrieveEntities
import crawler.retrievedata.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import crawler.retrievedata.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import crawler.retrievedata.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import crawler.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import database.MongoDatabaseService
import database.MongoDatabaseService.{SaveFBPage, SaveFBPost}
import reactivemongo.api.DefaultDB


/**
 * Created by roger on 11/03/15.
 */
object CrawlerWorker {
  def props(database: DefaultDB): Props =
    Props(new CrawlerWorker(database))
}

class CrawlerWorker(database: DefaultDB) extends Actor with ActorLogging {
  var retrievers: Set[ActorRef] = Set()
  //Maybe find better refactor to this...
  var mongoSaver: ActorRef = null

  def receive() = {
    case FetchDataSince(userId, accessToken, lastCrawled) =>
      val client = sender()
      val simpleParameters = FBSimpleParameters(Some(userId), Some(accessToken), since = lastCrawled)
      log.info("Fetching data for " + userId)

      val pageRetriever = context.actorOf(RetrieveLikedPages.props())
      pageRetriever ! RetrieveEntities(simpleParameters)
      retrievers += pageRetriever

      val postRetriever = context.actorOf(RetrievePosts.props())
      postRetriever ! RetrieveEntities(simpleParameters)
      retrievers += postRetriever

      val taggedRetriever = context.actorOf(RetrieveTaggedPosts.props())
      taggedRetriever ! RetrieveEntities(simpleParameters)
      retrievers += taggedRetriever

      mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      context.become(awaitResults(client, userId))
    case _ =>
      log.error("Crawler worker received an unexpected message.")
  }

  def awaitResults(client: ActorRef, userId: String): Receive = {

    case PartialLikedPagesResult(pages) =>
      mongoSaver ! SaveFBPage(pages.toList)

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      mongoSaver ! SaveFBPage(pages.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialPostsResult(posts) =>
      mongoSaver ! SaveFBPost(posts.toList)

    case FinishedRetrievingPosts(posts) =>
      log.info(s"Received posts for user: $userId")
      mongoSaver ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialTaggedPostsResult(posts) =>
      mongoSaver ! SaveFBPost(posts.toList)

    case FinishedRetrievingTaggedPosts(posts) =>
      log.info(s"Received tagged posts for user: $userId")
      mongoSaver ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case _ =>
      log.error("crawler worker received unexpected message for " + userId)
      client ! FinishedCrawling(userId)

  }

  def verifyDone(client: ActorRef, userId: String) = {
    if (retrievers.isEmpty) {
      client ! FinishedCrawling(userId)
    }
  }

  override def postStop(): Unit = {
    super.postStop()
    retrievers.foreach(r => r ! PoisonPill)

  }


}

