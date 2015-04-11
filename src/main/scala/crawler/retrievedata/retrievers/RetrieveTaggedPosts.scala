package crawler.retrievedata.retrievers


import akka.actor.{ActorRef, Props}
import crawler.common.GraphResponses.Post
import crawler.common.RetrieveEntitiesService
import crawler.common.RetrieveEntitiesService.{PartialResult, FinishedRetrievingEntities, NotEnoughFound, RetrieveEntities}
import crawler.retrievedata.RetrieveData
import crawler.retrievedata.retrievers.RetrieveTaggedPosts.{PartialTaggedPostsResult, FinishedRetrievingTaggedPosts}

/**
 * Created by roger on 05/03/15.
 */
object RetrieveTaggedPosts {
  def props(): Props =
    Props(new RetrieveTaggedPosts())

  case class FinishedRetrievingTaggedPosts(posts: Vector[Post])
  case class PartialTaggedPostsResult(posts: Vector[Post])

}

class RetrieveTaggedPosts extends RetrieveData {

  var entityCount = 0

  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"tagged?fields=id,from,message,created_time,likes.limit(1000).summary(true),source,place" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
  }

  def awaitResponse(client: ActorRef): Receive = {
    case PartialResult(entities) =>
      entityCount += entities.length
      client ! PartialTaggedPostsResult(entities.asInstanceOf[Vector[Post]])
    case FinishedRetrievingEntities(entities) =>
      entityCount += entities.length
      log.info(s"Received $entityCount tagged posts.")
      client ! FinishedRetrievingTaggedPosts(entities.asInstanceOf[Vector[Post]])
    case NotEnoughFound(entities) =>
      entityCount += entities.length
      log.info(s"Received not enough ($entityCount) tagged posts.")
      client ! FinishedRetrievingTaggedPosts(entities.asInstanceOf[Vector[Post]])
    case _ => log.error("RetrievingTaggedPosts received unexpected message")

  }
}
