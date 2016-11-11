package me.reminisce.fetching

import akka.actor._
import me.reminisce.analysis.DataAnalyser
import me.reminisce.analysis.DataAnalyser.{FinalAnalysis, TransientPostsAnalysis}
import me.reminisce.database.DeletionService.RemoveExtraLikes
import me.reminisce.database.MongoDatabaseService.{SaveFBPage, SaveFBPost}
import me.reminisce.database.{DeletionService, MongoDatabaseService}
import me.reminisce.fetching.FetcherService.{FetchDataSince, FinishedFetching}
import me.reminisce.fetching.FetcherWorker._
import me.reminisce.fetching.config.GraphResponses.{Friend, Page, Post}
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.RetrieveEntities
import me.reminisce.fetching.retrievers.RetrieveFriends.FinishedRetrievingFriends
import me.reminisce.fetching.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}
import me.reminisce.fetching.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}
import me.reminisce.fetching.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}
import me.reminisce.fetching.retrievers.{RetrieveFriends, RetrieveLikedPages, RetrievePosts, RetrieveTaggedPosts}
import me.reminisce.server.domain.Domain.Done
import reactivemongo.api.DefaultDB

/**
  * Factory for [[me.reminisce.fetching.FetcherWorker]] and useful methods
  */
object FetcherWorker {
  /**
    * Create a fetcher worker
    *
    * @param database database to store the fetched data
    * @return props for the created actor
    */
  def props(database: DefaultDB, userId: String): Props =
  Props(new FetcherWorker(database, userId))

  /**
    * Removes empty posts from the list
    *
    * @param posts posts to filter
    * @return filtered posts
    */
  def prunePosts(posts: Vector[Post]): Vector[Post] = {
    posts.filter(p => p.message.exists(_.nonEmpty) || p.story.exists(_.nonEmpty))
  }
}

/**
  * A fetcher worker, invokes the particular retrievers and request the storing of data in the database
  *
  * @param database the database to store data in
  */
class FetcherWorker(database: DefaultDB, userId: String) extends Actor with ActorLogging {

  val dataAnalyser = context.actorOf(DataAnalyser.props(userId, database))
  val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))

  /**
    * Actor entry point, handles the FetchDataSince(userId, accessToken, lastFetched) message by creating the data
    * retrievers and requesting the data retrieving
    *
    * @return Nothing
    */
  def receive = {
    case FetchDataSince(accessToken, lastFetched) =>
      val client = sender()
      val simpleParameters = FBParameters(Some(userId), Some(accessToken), since = lastFetched)
      log.info("Fetching data for " + userId)

      val pageRetriever = context.actorOf(RetrieveLikedPages.props())
      pageRetriever ! RetrieveEntities(simpleParameters)

      val postRetriever = context.actorOf(RetrievePosts.props())
      postRetriever ! RetrieveEntities(simpleParameters)

      val taggedRetriever = context.actorOf(RetrieveTaggedPosts.props())
      taggedRetriever ! RetrieveEntities(simpleParameters)

      val friendsRetriever = context.actorOf(RetrieveFriends.props())
      friendsRetriever ! RetrieveEntities(simpleParameters)

      val workers = Set(pageRetriever, postRetriever, taggedRetriever, friendsRetriever)
      context.become(awaitResults(client, userId, workers, Set(), Set(), Set()))
    case _ =>
      log.error("Fetcher worker received an unexpected message.")
  }

  /**
    * Waits results from the data retrievers. The parameters hold the current state of the actor. Handles the following
    * messages:
    * - PartialLikedPagesResult(pages): stores the pages in the database
    * - FinishedRetrievingLikedPages(pages): stores the pages in the database, removes extra likes (pages not liked
    * anymore) and tests if the fetching is done, if it is the case, report to the client.
    * - PartialPostsResult(posts): stores posts in the database, tests if the fetching is done, if it is the case,
    * report to the client.
    * - FinishedRetrievingPosts(posts): stores posts in the database, tests if the fetching is done, if it is the case,
    * report to the client.
    * - PartialTaggedPostsResult(posts): stores posts in the database, tests if the fetching is done, if it is the case,
    * report to the client.
    * - FinishedRetrievingTaggedPosts(posts): stores posts in the , tests if the fetching is done, if it is the case,
    * report to the client.
    * - Done(message): the deletion is done (see liked pages above), check if everything is done, if so report to client
    *
    * @param client     fetching requester
    * @param userId     user for which data has to be fetched
    * @param workers    fetching and deleting workers
    * @param foundPosts fetched posts
    * @param foundPages fetched pages
    * @return Nothing
    */
  private def awaitResults(client: ActorRef, userId: String, workers: Set[ActorRef], foundPosts: Set[String],
                           foundPages: Set[String], foundFriends : Set[Friend]): Receive = {

    case PartialLikedPagesResult(pages) =>
      val newFoundPages = pages.map(page => page.id).toSet ++ foundPages
      storePages(pages, userId)
      context.become(awaitResults(client, userId, workers, foundPosts, newFoundPages, foundFriends))

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      storePages(pages, userId)
      val deletionService = context.actorOf(DeletionService.props(database))
      deletionService ! RemoveExtraLikes(userId, foundPages)
      val newFoundPages = pages.map(page => page.id).toSet ++ foundPages
      verifyDone(client, userId, workers, Set(deletionService), Set(sender()), foundPosts, newFoundPages, foundFriends)


    case PartialPostsResult(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, foundFriends, partial = true)

    case FinishedRetrievingPosts(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, foundFriends)

    case PartialTaggedPostsResult(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, foundFriends, partial = true)

    case FinishedRetrievingTaggedPosts(posts) =>
      handlePosts(client, workers, posts, userId, foundPosts, foundPages, foundFriends)

    case Done(message) =>
      log.info(message)
      verifyDone(client, userId, workers, Set(), Set(sender()), foundPosts, foundPages, foundFriends)

    case FinishedRetrievingFriends(friends) =>
      verifyDone(client, userId, workers, Set(), Set(sender()), foundPosts, foundPages, friends)

    case _ =>
      log.error("Fetcher worker received unexpected message for " + userId)
      client ! FinishedFetching(userId)

  }

  /**
    * Verifies if the work is done, if so report to client and requests to aggregate the user summary
    *
    * @param client     fetching requester
    * @param userId     user for which the data is fetched
    * @param workers    current workers
    * @param newWorkers workers to add
    * @param oldWorkers workers to remove
    * @param foundPosts posts fetched
    * @param foundPages pages fetched
    */
  private def verifyDone(client: ActorRef, userId: String, workers: Set[ActorRef], newWorkers: Set[ActorRef],
                         oldWorkers: Set[ActorRef], foundPosts: Set[String], foundPages: Set[String],
                         foundFriends: Set[Friend]) = {
    val newWorkersSet = workers ++ newWorkers -- oldWorkers
    if (newWorkersSet.isEmpty) {
      client ! FinishedFetching(userId)
      dataAnalyser ! FinalAnalysis(foundPosts, foundPages, foundFriends)
    }
    context.become(awaitResults(client, userId, newWorkersSet, foundPosts, foundPages, foundFriends))
  }


  /**
    * Cascades stop to children
    */
  override def postStop(): Unit = {
    super.postStop()
    context.children.foreach(r => r ! PoisonPill)
  }

  /**
    * Store posts
    *
    * @param prunedPosts posts with text (other removed by [[me.reminisce.fetching.FetcherWorker.prunePosts]])
    * @param userId      id of the user for which data is being fetched
    */
  private def storePosts(prunedPosts: Vector[Post], userId: String): Unit = {
    dataAnalyser ! TransientPostsAnalysis(prunedPosts.toList)
    mongoSaver ! SaveFBPost(prunedPosts.toList)
  }


  /**
    * Prunes posts, stores them, stores summary on them and then tests if the fetching is done, if it is the case,
    * report to the client.
    *
    * @param client     fetching requester
    * @param workers    current active workers
    * @param posts      posts to handle
    * @param userId     user for which data is being fetched
    * @param foundPosts already handled posts
    * @param foundPages already handled pages
    * @param partial    is the result partial
    */
  private def handlePosts(client: ActorRef, workers: Set[ActorRef], posts: Vector[Post], userId: String,
                          foundPosts: Set[String], foundPages: Set[String], foundFriends: Set[Friend],
                          partial: Boolean = false): Unit = {
    val prunedPosts = prunePosts(posts)
    storePosts(prunedPosts, userId)
    val oldWorkers = if (partial) {
      Set[ActorRef]()
    } else {
      Set(sender())
    }
    verifyDone(client, userId, workers, Set(), oldWorkers, foundPosts ++ prunedPosts.map(p => p.id), foundPages, foundFriends)
  }

  /**
    * Store pages
    *
    * @param pages  pages to store
    * @param userId user for which data is being fetched
    */
  private def storePages(pages: Vector[Page], userId: String): Unit = {
    mongoSaver ! SaveFBPage(pages.toList)
  }
}

