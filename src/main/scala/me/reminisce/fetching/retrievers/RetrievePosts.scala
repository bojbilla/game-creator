package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetching.config.GraphResponses.Post
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, PartialResult, RetrieveEntities}
import me.reminisce.fetching.retrievers.RetrievePosts.{FinishedRetrievingPosts, PartialPostsResult}

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrievePosts]] and case classes definition for message passing
  */
object RetrievePosts {
  /**
    * Creates a retrieve posts actor
    * @return props for the created actor
    */
  def props(): Props =
    Props(new RetrievePosts())

  case class FinishedRetrievingPosts(posts: Vector[Post])

  case class PartialPostsResult(posts: Vector[Post])

}

class RetrievePosts extends RetrieveData {

  /**
    * Entry point for this actor, handles the RetrieveEntities(params) message by creating a RetrieveEntitiesService
    * with the suitable parameters and requesting a data retrieval
    * @return Nothing
    */
  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"posts?fields=reactions.limit(1000).summary(true),message,created_time,link,from,type,story,source," +
        s"full_picture,attachments{id,description,media{image},type},place," +
        s"comments.limit(1000).summary(true){created_time,from,message,attachment,like_count}" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
    case _ => log.error("RetrievingTaggedPosts received unexpected message")
  }

  /**
    * Awaits the response from the RetrieveEntitiesService. Handles the following messages:
    * - PartialResult(entities): a partial result, sends it back to client
    * - FinishedRetrievingEntities(entities): last retrieved entities, sends it to client
    * - NotEnoughData(entities): not enough data was found, sends the found ones to client
    * @param client original requester
    * @param entityCount current retrieved entities count
    * @return Nothing
    */
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