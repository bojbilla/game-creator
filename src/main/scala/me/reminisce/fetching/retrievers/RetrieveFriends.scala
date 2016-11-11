package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.fetching.FBParameters
import me.reminisce.fetching.config.GraphResponses.Friend
import me.reminisce.fetching.retrievers.RetrieveEntitiesService.{FinishedRetrievingEntities, PartialResult, RetrieveEntities, RetrieveError}
import me.reminisce.fetching.retrievers.RetrieveFriends.FinishedRetrievingFriends

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrieveFriends]] and case classes definition for message passing
  */
object RetrieveFriends {
  /**
    * Creates a retrieve friends actor
    *
    * @return props for the created actor
    */
  def props(): Props = Props(new RetrieveFriends())

  case class FinishedRetrievingFriends(friends: Set[Friend])

}


class RetrieveFriends extends RetrieveData {

  /**
    * Entry point for this actor, handles the RetrieveEntities(params) message by creating a RetrieveEntitiesService
    * with the suitable parameters and requesting a data retrieval
    *
    * @return Nothing
    */
  def receive = {
    case RetrieveEntities(params) =>
      startRetriever("invitable_friends", params)
      val client = sender()
      context.become(awaitInvitableFriends(client, params))
    case _ => log.error("RetrieveFriends received unexpected message")
  }

  /**
    * Awaits the response from the RetrieveEntitiesService. Handles the following messages:
    * - PartialResult(entities): a partial result, sends it back to client
    * - FinishedRetrievingEntities(entities): last retrieved entities, sends it to client
    * - RetrieveError(message): error while retrieving
    *
    * @param client           original requester
    * @param params           facebook parameters for retrieval
    * @param friendsRetrieved current retrieved friends
    * @return Nothing
    */
  private def awaitInvitableFriends(client: ActorRef, params: FBParameters, friendsRetrieved: Set[Friend] = Set()): Receive = {
    case PartialResult(entities) =>
      context.become(awaitInvitableFriends(client, params, friendsRetrieved ++ entities.asInstanceOf[Vector[Friend]]))
    case FinishedRetrievingEntities(entities) =>
      sender() ! PoisonPill
      startRetriever("friends", params)
      context.become(awaitFriends(client, friendsRetrieved ++ entities.asInstanceOf[Vector[Friend]]))
    case RetrieveError(message) =>
      log.info(s"Retrieval error : $message")
      client ! FinishedRetrievingFriends(friendsRetrieved)
      sender() ! PoisonPill
    case _ => log.error("RetrieveInvitableFriends received unexpected message")
  }

  /**
    * Awaits the response from the RetrieveEntitiesService. Handles the following messages:
    * - PartialResult(entities): a partial result, sends it back to client
    * - FinishedRetrievingEntities(entities): last retrieved entities, sends it to client
    * - RetrieveError(message): error while retrieving
    *
    * @param client           original requester
    * @param friendsRetrieved current retrieved friends
    * @return Nothing
    */
  private def awaitFriends(client: ActorRef, friendsRetrieved: Set[Friend] = Set()): Receive = {
    case PartialResult(entities) =>
      context.become(awaitFriends(client, friendsRetrieved ++ entities.asInstanceOf[Vector[Friend]]))
    case FinishedRetrievingEntities(entities) =>
      val finalSet = friendsRetrieved ++ entities.asInstanceOf[Vector[Friend]]
      log.info(s"Received ${friendsRetrieved.size} friends.")
      client ! FinishedRetrievingFriends(finalSet)
      sender() ! PoisonPill
    case RetrieveError(message) =>
      log.info(s"Retrieval error : $message")
      client ! FinishedRetrievingFriends(friendsRetrieved)
      sender() ! PoisonPill
    case _ => log.error("RetrieveInvitableFriends received unexpected message")
  }

  private def startRetriever(route: String, params: FBParameters): Unit = {
    val params1 = params.copy(query = Some(s"me/$route?fields=id,name"))
    val retriever = context.actorOf(RetrieveEntitiesService.props[Friend](defaultFilter[Friend]))
    retriever ! RetrieveEntities(params1)
  }
}
