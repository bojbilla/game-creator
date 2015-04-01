package crawler

import akka.actor._
import crawler.CrawlerService.{FetchData, FinishedCrawling}
import crawler.common.FBSimpleParameters
import crawler.common.RetrieveEntitiesService.RetrieveEntities
import crawler.retrievedata.retrievers.RetrieveLikedPages.FinishedRetrievingLikedPages
import crawler.retrievedata.retrievers.RetrievePosts.FinishedRetrievingPosts
import crawler.retrievedata.retrievers.RetrieveTaggedPosts.FinishedRetrievingTaggedPosts
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

  def receive() = {
    case FetchData(userId, accessToken) =>
      val client = sender()
      val simpleParameters = FBSimpleParameters(Some(userId), Some(accessToken))
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
  }

  def awaitResults(client: ActorRef, userId: String): Receive = {

    case FinishedRetrievingLikedPages(pages) =>
      log.info(s"Received liked pages for user: $userId")
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveFBPage(pages.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case FinishedRetrievingPosts(posts) =>
      log.info(s"Received posts for user: $userId")
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)

    case FinishedRetrievingTaggedPosts(posts) =>
      log.info(s"Received tagged posts for user: $userId")
      val mongoSaver = context.actorOf(MongoDatabaseService.props(userId, database))
      mongoSaver ! SaveFBPost(posts.toList)
      retrievers -= sender()
      verifyDone(client, userId)


    case _ =>
      log.error("crawler worker received unexpected message for " + userId)
      client ! FinishedCrawling(userId)

  }

  def verifyDone(client: ActorRef, userId: String) = {
    if (retrievers.isEmpty){
      client ! FinishedCrawling(userId)
    }
  }

  override def postStop(): Unit = {
    super.postStop()
    retrievers.foreach(r => r ! PoisonPill)

  }


}

