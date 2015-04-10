package crawler.retrievedata.retrievers


import akka.actor.{ActorRef, Props}
import crawler.common.GraphResponses.Page
import crawler.common.RetrieveEntitiesService
import crawler.common.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, RetrieveEntities}
import crawler.retrievedata.RetrieveData
import crawler.retrievedata.retrievers.RetrieveLikedPages.FinishedRetrievingLikedPages

/**
 * Created by roger on 05/03/15.
 */

object RetrieveLikedPages {
  def props(): Props =
    Props(new RetrieveLikedPages())

  case class FinishedRetrievingLikedPages(pages: Vector[Page])

}


class RetrieveLikedPages extends RetrieveData {

  def receive = {
    case RetrieveEntities(params) =>
      val params1 = params.copy(query = Some("me/likes?fields=name,photos.type(profile).limit(1){id,name,source}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Page](defaultFilter[Page]))
      retriever ! RetrieveEntities(params1)
      val client = sender()
      context.become(awaitResponse(client))
  }

  def awaitResponse(client: ActorRef): Receive = {
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entities.length} liked pages.")
      client ! FinishedRetrievingLikedPages(entities.asInstanceOf[Vector[Page]])
    case NotEnoughFound(entities) =>
      log.info(s"Received not enough (${entities.length}) liked pages.")
      client ! FinishedRetrievingLikedPages(entities.asInstanceOf[Vector[Page]])

  }
}
