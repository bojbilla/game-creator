package me.reminisce.fetcher.retrievedata.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetcher.common.GraphResponses.Post
import me.reminisce.fetcher.common.RetrieveEntitiesService
import me.reminisce.fetcher.common.RetrieveEntitiesService.{FinishedRetrievingEntities, NotEnoughFound, PartialResult, RetrieveEntities}
import me.reminisce.fetcher.retrievedata.RetrieveData
import me.reminisce.fetcher.retrievedata.retrievers.RetrieveTaggedPosts.{FinishedRetrievingTaggedPosts, PartialTaggedPostsResult}

object RetrieveTaggedPosts {
  def props(): Props =
    Props(new RetrieveTaggedPosts())

  case class FinishedRetrievingTaggedPosts(posts: Vector[Post])

  case class PartialTaggedPostsResult(posts: Vector[Post])

}

class RetrieveTaggedPosts extends RetrieveData {

  def receive = {
    case RetrieveEntities(params) =>
      val client = sender()
      val params1 = params.copy(query = Some(s"${params.userId.getOrElse("me")}/" +
        s"tagged?fields=id,message,created_time,link,from,likes.limit(1000).summary(true),source,place" +
        s"&since=${params.getSince}&until=${params.getUntil}"))
      val retriever = context.actorOf(RetrieveEntitiesService.props[Post](defaultFilter[Post]))
      retriever ! RetrieveEntities(params1)
      context.become(awaitResponse(client))
    case any =>
      log.error(s"Uknown message : $any.")
  }

  def awaitResponse(client: ActorRef, entityCount: Int = 0): Receive = {
    case PartialResult(entities) =>
      context.become(awaitResponse(client, entityCount + entities.length))
      client ! PartialTaggedPostsResult(entities.asInstanceOf[Vector[Post]])
    case FinishedRetrievingEntities(entities) =>
      log.info(s"Received ${entityCount + entities.length} tagged posts.")
      client ! FinishedRetrievingTaggedPosts(entities.asInstanceOf[Vector[Post]])
    case NotEnoughFound(entities) =>
      log.info(s"Received not enough (${entityCount + entities.length}) tagged posts.")
      client ! FinishedRetrievingTaggedPosts(entities.asInstanceOf[Vector[Post]])
    case _ => log.error("RetrievingTaggedPosts received unexpected message")

  }
}
