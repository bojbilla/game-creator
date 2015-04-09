package crawler.retrievedata.retrievers


import akka.actor.{ActorRef, Props}
import crawler.common.GraphResponses.Post
import crawler.common.RetrieveEntitiesService
import crawler.common.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, RetrieveEntities}
import crawler.retrievedata.RetrieveData
import crawler.retrievedata.retrievers.RetrievePosts.FinishedRetrievingPosts

object RetrievePosts{
  def props(): Props =
    Props(new RetrievePosts())

  case class FinishedRetrievingPosts(posts: Vector[Post])
}

class RetrievePosts extends RetrieveData{
  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"posts?fields=likes.limit(1000).summary(true),message,type,story,source," +
        s"full_picture,attachments{id,description,media{image},type}," +
        s"comments.limit(1000).summary(true){created_time,from,message,attachment,like_count}" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
  }

  def awaitResponse(client: ActorRef): Receive = {
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entities.length} posts.")
      client ! FinishedRetrievingPosts(entities.asInstanceOf[Vector[Post]])
    case NotEnoughFound(entities) =>
      log.info(s"Received not enough (${entities.length}) posts.")
      client ! FinishedRetrievingPosts(entities.asInstanceOf[Vector[Post]])
    case _ => log.error("RetrievingTaggedPosts received unexpected message")

  }
}