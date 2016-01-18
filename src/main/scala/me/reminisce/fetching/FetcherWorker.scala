package me.reminisce.fetching

import akka.actor._
import me.reminisce.database.DeletionService.RemoveExtraLikes
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost}
import me.reminisce.database.{DeletionService, MongoDatabaseService}
import me.reminisce.fetching.FetcherService.{FetchDataSince, FinishedFetching}
import me.reminisce.fetching.FetcherWorker._
import me.reminisce.fetching.config.GraphResponses
import me.reminisce.fetching.config.GraphResponses.{Page, Post}
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.RetrieveEntities
import me.reminisce.fetching.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import me.reminisce.fetching.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import me.reminisce.fetching.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import me.reminisce.fetching.retrievers.{RetrieveEntitiesService, RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import me.reminisce.server.domain.Domain.Done
import me.reminisce.stats.StatsHandler
import me.reminisce.stats.StatsHandler.{FinalStats, TransientPostsStats}
import reactivemongo.api.DefaultDB


object FetcherWorker {
  def props(database: DefaultDB): Props =
    Props(new FetcherWorker(database))

  def prunePosts(posts: Vector[Post]): Vector[Post] = {
    posts.filter(p => p.message.exists(_.nonEmpty) || p.story.exists(_.nonEmpty))
  }
}

class FetcherWorker(database: DefaultDB) extends Actor with ActorLogging {

  def receive = {
    case FetchDataSince(userId, accessToken, lastFetched) =>
      val client = sender()
      val simpleParameters = FBSimpleParameters(Some(userId), Some(accessToken), since = lastFetched)
      log.info("Fetching data for " + userId)

      val pageRetriever = context.actorOf(RetrieveLikedPages.props())
      pageRetriever ! RetrieveEntities(simpleParameters)

      val postRetriever = context.actorOf(RetrievePosts.props())
      postRetriever ! RetrieveEntities(simpleParameters)

      val taggedRetriever = context.actorOf(RetrieveTaggedPosts.props())
      taggedRetriever ! RetrieveEntities(simpleParameters)

      val workers = Set(pageRetriever, postRetriever, taggedRetriever)
      context.become(awaitResults(client, userId, workers, Set(), Set()))
    case _ =>
      log.error("Fetcher worker received an unexpected message.")
  }

  private def awaitResults(client: ActorRef, userId: String, workers: Set[ActorRef], foundPosts: Set[String], foundPages: Set[String]): Receive = {

    case PartialLikedPagesResult(pages) =>
      val newFoundPages = pages.map(page => page.id).toSet ++ foundPages
      storePages(pages, userId)
      context.become(awaitResults(client, userId, workers, foundPosts, newFoundPages))

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      storePages(pages, userId)
      val deletionService = context.actorOf(DeletionService.props(database))
      deletionService ! RemoveExtraLikes(userId, foundPages)
      val newFoundPages = pages.map(page => page.id).toSet ++ foundPages
      verifyDone(client, userId, workers, Set(deletionService), Set(sender()), foundPosts, newFoundPages)


    case PartialPostsResult(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, partial = true)

    case FinishedRetrievingPosts(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages)


    case PartialTaggedPostsResult(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, partial = true)

    case FinishedRetrievingTaggedPosts(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages)

    case Done(message) =>
      log.info(message)
      verifyDone(client, userId, workers, Set(), Set(sender()), foundPosts, foundPages)

    case _ =>
      log.error("Fetcher worker received unexpected message for " + userId)
      client ! FinishedFetching(userId)

  }

  private def verifyDone(client: ActorRef, userId: String, workers: Set[ActorRef], newWorkers: Set[ActorRef],
                         oldWorkers: Set[ActorRef], foundPosts: Set[String], foundPages: Set[String]) = {
    val newWorkersSet = workers ++ newWorkers -- oldWorkers
    if (newWorkersSet.isEmpty) {
      client ! FinishedFetching(userId)
      statsHandler(userId) ! FinalStats(foundPosts, foundPages)
    }
    context.become(awaitResults(client, userId, newWorkersSet, foundPosts, foundPages))
  }


  override def postStop(): Unit = {
    super.postStop()
    context.children.foreach(r => r ! PoisonPill)
  }

  private def mongoSaver(userId: String): ActorRef = {
    context.actorOf(MongoDatabaseService.props(userId, database))
  }

  private def statsHandler(userId: String): ActorRef = {
    context.actorOf(StatsHandler.props(userId, database))
  }

  private def storePosts(prunedPosts: Vector[Post], userId: String): Unit = {
    statsHandler(userId) ! TransientPostsStats(prunedPosts.toList)
    mongoSaver(userId) ! SaveFBPost(prunedPosts.toList)
  }

  private def handlePosts(client: ActorRef, workers: Set[ActorRef], posts: Vector[Post], userId: String,
                          foundPosts: Set[String], foundPages: Set[String], partial: Boolean = false): Unit = {
    val prunedPosts = prunePosts(posts)
    storePosts(prunedPosts, userId)
    val oldWorkers = if (partial) {
      Set[ActorRef]()
    } else {
      Set(sender())
    }
    verifyDone(client, userId, workers, Set(), oldWorkers, foundPosts ++ prunedPosts.map(p => p.id), foundPages)
  }

  private def storePages(pages: Vector[Page], userId: String): Unit = {
    mongoSaver(userId) ! SaveFBPage(pages.toList)
  }
}

