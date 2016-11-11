package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.fetching.config.GraphResponses.Post
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.{FinishedRetrievingEntities, RetrieveError, PartialResult, RetrieveEntities}
import me.reminisce.fetching.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrieveTaggedPosts]] and case classes definition for message passing
  */
object RetrieveTaggedPosts {
  /**
    * Creates a retrieve tagged posts actor
    *
    * @return props for the created actor
    */
  def props(): Props =
  Props(new RetrieveTaggedPosts())

  case class FinishedRetrievingTaggedPosts(posts: Vector[Post])

  case class PartialTaggedPostsResult(posts: Vector[Post])

}

class RetrieveTaggedPosts extends RetrieveData {

  /**
    * Entry point for this actor, handles the RetrieveEntities(params) message by creating a RetrieveEntitiesService
    * with the suitable parameters and requesting a data retrieval
    *
    * @return Nothing
    */
  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"tagged?fields=id,message,created_time,link,from,reactions.limit(1000).summary(true),source,place" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
    case any =>
      log.error(s"Unknown message : $any.")
  }

  /**
    * Awaits the response from the RetrieveEntitiesService. Handles the following messages:
    * - PartialResult(entities): a partial result, sends it back to client
    * - FinishedRetrievingEntities(entities): last retrieved entities, sends it to client
    * - RetrieveError(message): error while retrieving
    *
    * @param client      original requester
    * @param entityCount current retrieved entities count
    * @return Nothing
    */
  private def awaitResponse(client: ActorRef, entityCount: Int = 0): Receive = {
    case PartialResult(entities) =>
      context.become(awaitResponse(client, entityCount + entities.length))
      client ! PartialTaggedPostsResult(entities.asInstanceOf[Vector[Post]])
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entityCount + entities.length} tagged posts.")
      client ! FinishedRetrievingTaggedPosts(entities.asInstanceOf[Vector[Post]])
      sender() ! PoisonPill
    case RetrieveError(message) =>
      log.info(s"Retrieval error : $message")
      client ! FinishedRetrievingTaggedPosts(Vector())
      sender() ! PoisonPill
    case _ => log.error("RetrievingTaggedPosts received unexpected message")

  }
}
