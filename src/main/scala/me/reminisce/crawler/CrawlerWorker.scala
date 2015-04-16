package me.reminisce.crawler

import akka.actor._
import me.reminisce.crawler.CrawlerService.{FetchDataSince, FinishedCrawling}
import me.reminisce.crawler.common.{FBSimpleParameters, RetrieveEntitiesService}
import me.reminisce.crawler.common.RetrieveEntitiesService.RetrieveEntities
import me.reminisce.crawler.retrievedata.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import me.reminisce.crawler.retrievedata.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import me.reminisce.crawler.retrievedata.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import me.reminisce.crawler.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.MongoDatabaseService.{SaveFBPost, SaveFBPage}
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

      context.become(awaitResults(client, userId))
    case _ =>
      log.error("Crawler worker received an unexpected message.")
  }

  def awaitResults(client: ActorRef, userId: String): Receive = {

    case PartialLikedPagesResult(pages) =>
      mongoSaver(userId) ! SaveFBPage(pages.toList)

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      mongoSaver(userId) ! SaveFBPage(pages.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialPostsResult(posts) =>
      mongoSaver(userId) ! SaveFBPost(posts.toList)

    case FinishedRetrievingPosts(posts) =>
      log.info(s"Received posts for user: $userId")
      mongoSaver(userId) ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialTaggedPostsResult(posts) =>
      mongoSaver(userId) ! SaveFBPost(posts.toList)

    case FinishedRetrievingTaggedPosts(posts) =>
      log.info(s"Received tagged posts for user: $userId")
      mongoSaver(userId) ! SaveFBPost(posts.toList)
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

  def mongoSaver(userId: String): ActorRef = {
    context.actorOf(MongoDatabaseService.props(userId, database))
  }

}

