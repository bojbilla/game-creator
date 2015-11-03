package me.reminisce.fetcher

import akka.actor._
import me.reminisce.database.DeletionService.RemoveExtraLikes
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost}
import me.reminisce.database.{DeletionService, MongoDatabaseService}
import me.reminisce.fetcher.FetcherService.{FetchDataSince, FinishedFetching}
import me.reminisce.fetcher.FetcherWorker._
import me.reminisce.fetcher.common.FBSimpleParameters
import me.reminisce.fetcher.common.GraphResponses.{Page, Post}
import me.reminisce.fetcher.common.RetrieveEntitiesService.RetrieveEntities
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import me.reminisce.fetcher.retrievedata.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import me.reminisce.fetcher.retrievedata.retrievers.{RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import me.reminisce.server.domain.Domain.Done
import me.reminisce.service.stats.StatsHandler
import me.reminisce.service.stats.StatsHandler.{FinalStats, TransientPostsStats}
import reactivemongo.api.DefaultDB


object FetcherWorker {
  def props(database: DefaultDB): Props =
    Props(new FetcherWorker(database))

  def prunePosts(posts: Vector[Post]): Vector[Post] = {
    posts.filter(p => p.message.exists(_.nonEmpty) || p.story.exists(_.nonEmpty))
  }
}

class FetcherWorker(database: DefaultDB) extends Actor with ActorLogging {
  var workers: Set[ActorRef] = Set()
  var foundPosts: Set[String] = Set()
  var foundPages: Set[String] = Set()

  def receive = {
    case FetchDataSince(userId, accessToken, lastFetched) =>
      val client = sender()
      val simpleParameters = FBSimpleParameters(Some(userId), Some(accessToken), since = lastFetched)
      log.info("Fetching data for " + userId)

      val pageRetriever = context.actorOf(RetrieveLikedPages.props())
      pageRetriever ! RetrieveEntities(simpleParameters)
      workers += pageRetriever

      val postRetriever = context.actorOf(RetrievePosts.props())
      postRetriever ! RetrieveEntities(simpleParameters)
      workers += postRetriever

      val taggedRetriever = context.actorOf(RetrieveTaggedPosts.props())
      taggedRetriever ! RetrieveEntities(simpleParameters)
      workers += taggedRetriever

      context.become(awaitResults(client, userId))
    case _ =>
      log.error("Fetcher worker received an unexpected message.")
  }

  def awaitResults(client: ActorRef, userId: String): Receive = {

    case PartialLikedPagesResult(pages) =>
      storePages(pages, userId)

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      storePages(pages, userId)
      val deletionService = context.actorOf(DeletionService.props(database))
      deletionService ! RemoveExtraLikes(userId, foundPages)
      workers += deletionService
      done(sender(), client, userId)


    case PartialPostsResult(posts) =>
      storePosts(posts, userId)

    case FinishedRetrievingPosts(posts) =>
      log.info(s"Received posts for user: $userId")
      storePosts(posts, userId)
      done(sender(), client, userId)


    case PartialTaggedPostsResult(posts) =>
      storePosts(posts, userId)

    case FinishedRetrievingTaggedPosts(posts) =>
      log.info(s"Received tagged posts for user: $userId")
      storePosts(posts, userId)
      done(sender(), client, userId)

    case Done(message) =>
      log.info(message)
      done(sender(), client, userId)

    case _ =>
      log.error("Fetcher worker received unexpected message for " + userId)
      client ! FinishedFetching(userId)

  }

  def verifyDone(client: ActorRef, userId: String) = {
    if (workers.isEmpty) {
      client ! FinishedFetching(userId)
      statsHandler(userId) ! FinalStats(foundPosts, foundPages)
    }
  }

  private def done(worker: ActorRef, client: ActorRef, userId: String): Unit = {
    workers -= worker
    verifyDone(client, userId)
  }

  override def postStop(): Unit = {
    super.postStop()
    workers.foreach(r => r ! PoisonPill)
  }

  def mongoSaver(userId: String): ActorRef = {
    context.actorOf(MongoDatabaseService.props(userId, database))
  }

  def statsHandler(userId: String): ActorRef = {
    context.actorOf(StatsHandler.props(userId, database))
  }

  private def storePosts(posts: Vector[Post], userId: String): Unit = {
    val prunedPosts = prunePosts(posts)
    foundPosts ++= prunedPosts.map(post => post.id).toSet
    statsHandler(userId) ! TransientPostsStats(prunedPosts.toList)
    mongoSaver(userId) ! SaveFBPost(prunedPosts.toList)
  }

  private def storePages(pages: Vector[Page], userId: String): Unit = {
    foundPages ++= pages.map(page => page.id).toSet
    mongoSaver(userId) ! SaveFBPage(pages.toList)
  }

}

