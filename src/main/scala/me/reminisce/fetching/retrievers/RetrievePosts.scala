package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetching.config.GraphResponses.Post
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, PartialResult, RetrieveEntities}
import me.reminisce.fetching.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}

object RetrievePosts {
  def props(): Props =
    Props(new RetrievePosts())

  case class FinishedRetrievingPosts(posts: Vector[Post])

  case class PartialPostsResult(posts: Vector[Post])

}

class RetrievePosts extends RetrieveData {

  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"posts?fields=likes.limit(1000).summary(true),message,link,from,type,story,source," +
        s"full_picture,attachments{id,description,media{image},type},place," +
        s"comments.limit(1000).summary(true){created_time,from,message,attachment,like_count}" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
    case _ => log.error("RetrievingTaggedPosts received unexpected message")
  }

  private def awaitResponse(client: ActorRef, entityCount: Int = 0): Receive = {
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entityCount + entities.length} posts.")
      client ! FinishedRetrievingPosts(entities.asInstanceOf[Vector[Post]])
    case PartialResult(entities) =>
      context.become(awaitResponse(client, entityCount + entities.length))
      client ! PartialPostsResult(entities.asInstanceOf[Vector[Post]])
    case NotEnoughFound(entities) =>
      log.info(s"Received not enough (${entityCount + entities.length}) posts.")
      client ! FinishedRetrievingPosts(entities.asInstanceOf[Vector[Post]])
    case _ => log.error("RetrievingTaggedPosts received unexpected message")

  }
}