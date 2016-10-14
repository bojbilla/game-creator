package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetching.config.GraphResponses.Page
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, PartialResult, RetrieveEntities}
import me.reminisce.fetching.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrieveLikedPages]] and case classes definition for message passing
  */
object RetrieveLikedPages {
  /**
    * Creates a retrieve liked pages actor
    * @return props for the created actor
    */
  def props(): Props =
    Props(new RetrieveLikedPages())

  case class FinishedRetrievingLikedPages(pages: Vector[Page])

  case class PartialLikedPagesResult(pages: Vector[Page])

}


class RetrieveLikedPages extends RetrieveData {

  /**
    * Entry point for this actor, handles the RetrieveEntities(params) message by creating a RetrieveEntitiesService
    * with the suitable parameters and requesting a data retrieval
    * @return Nothing
    */
  def receive = {
    case RetrieveEntities(params) =>
      val params1 = params.copy(query = Some("me/likes?fields=name,photos.type(profile).limit(1){id,name,source},fan_count,created_time"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Page](defaultFilter[Page]))
      retriever ! RetrieveEntities(params1)
      val client = sender()
      context.become(awaitResponse(client))
    case _ => log.error("RetrieveLikedPages received unexpected message")
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
    case PartialResult(entities) =>
      context.become(awaitResponse(client, entityCount + entities.length))
      client ! PartialLikedPagesResult(entities.asInstanceOf[Vector[Page]])
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entityCount + entities.length} liked pages.")
      client ! FinishedRetrievingLikedPages(entities.asInstanceOf[Vector[Page]])
    case NotEnoughFound(entities) =>
      log.info(s"Received not enough (${entityCount + entities.length}) liked pages.")
      client ! FinishedRetrievingLikedPages(entities.asInstanceOf[Vector[Page]])
    case _ => log.error("RetrieveLikedPages received unexpected message")
  }
}
