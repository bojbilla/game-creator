package me.reminisce.fetcher.retrievedata.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetcher.common.GraphResponses.Page
import me.reminisce.fetcher.common.RetrieveEntitiesService
import me.reminisce.fetcher.common.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, PartialResult, RetrieveEntities}
import me.reminisce.fetcher.retrievedata.RetrieveData
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveLikedPages.{FinishedRetrievingLikedPages, PartialLikedPagesResult}


object RetrieveLikedPages {
  def props(): Props =
    Props(new RetrieveLikedPages())

  case class FinishedRetrievingLikedPages(pages: Vector[Page])

  case class PartialLikedPagesResult(pages: Vector[Page])

}


class RetrieveLikedPages extends RetrieveData {

  def receive = {
    case RetrieveEntities(params) =>
      val params1 = params.copy(query = Some("me/likes?fields=name,photos.type(profile).limit(1){id,name,source},likes,created_time"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Page](defaultFilter[Page]))
      retriever ! RetrieveEntities(params1)
      val client = sender()
      context.become(awaitResponse(client))
    case _ => log.error("RetrieveLikedPages received unexpected message")
  }

  def awaitResponse(client: ActorRef, entityCount: Int = 0): Receive = {
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
