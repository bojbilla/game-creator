package me.reminisce.fetcher

import akka.actor._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost, SavePagesStats, SavePostStats}
import me.reminisce.fetcher.FetcherService.{FetchDataSince, FinishedFetching}
import me.reminisce.fetcher.common.FBSimpleParameters
import me.reminisce.fetcher.common.RetrieveEntitiesService.RetrieveEntities
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import me.reminisce.fetcher.retrievedata.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import me.reminisce.fetcher.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import reactivemongo.api.DefaultDB


/**
 * Created by roger on 11/03/15.
 */
object FetcherWorker {
  def props(database: DefaultDB): Props =
    Props(new FetcherWorker(database))
}

class FetcherWorker(database: DefaultDB) extends Actor with ActorLogging {
  var retrievers: Set[ActorRef] = Set()
  var foundPosts: Set[String] = Set()

  def receive() = {
    case FetchDataSince(userId, accessToken, lastFetched) =>
      val client = sender()
      val simpleParameters = FBSimpleParameters(Some(userId), Some(accessToken), since = lastFetched)
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
      log.error("Fetcher worker received an unexpected message.")
  }

  def awaitResults(client: ActorRef, userId: String): Receive = {

    case PartialLikedPagesResult(pages) =>
      mongoSaver(userId) ! SaveFBPage(pages.toList)

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      mongoSaver(userId) ! SaveFBPage(pages.toList)
      mongoSaver(userId) ! SavePagesStats
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialPostsResult(posts) =>
      foundPosts ++= posts.map(post => post.id).toSet
      mongoSaver(userId) ! SaveFBPost(posts.toList)

    case FinishedRetrievingPosts(posts) =>
      log.info(s"Received posts for user: $userId")
      foundPosts ++= posts.map(post => post.id).toSet
      mongoSaver(userId) ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case PartialTaggedPostsResult(posts) =>
      foundPosts ++= posts.map(post => post.id).toSet
      mongoSaver(userId) ! SaveFBPost(posts.toList)

    case FinishedRetrievingTaggedPosts(posts) =>
      log.info(s"Received tagged posts for user: $userId")
      foundPosts ++= posts.map(post => post.id).toSet
      mongoSaver(userId) ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case _ =>
      log.error("Fetcher worker received unexpected message for " + userId)
      client ! FinishedFetching(userId)

  }

  def verifyDone(client: ActorRef, userId: String) = {
    if (retrievers.isEmpty) {
      client ! FinishedFetching(userId)
      mongoSaver(userId) ! SavePostStats(foundPosts.toList)
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

