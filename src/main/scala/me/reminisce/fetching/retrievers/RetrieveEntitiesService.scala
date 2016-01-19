package me.reminisce.fetching.retrievers

import akka.actor.{ActorRef, Props}
import me.reminisce.fetching.config.GraphResponses.{Paging, Root}
import me.reminisce.fetching.retrievers.RetrieveEntitiesService._
import me.reminisce.fetching.{FBCommunicationManager, FBParameters}
import me.reminisce.server.domain.RestMessage
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods._
import spray.client.pipelining._
import spray.http.StatusCodes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrieveEntitiesService]] and case classes for message passing
  */
object RetrieveEntitiesService {

  //Used for starting to fetch data on facebook
  case class RetrieveEntities(params: FBParameters) extends RestMessage

  //Will be sent if not enough entities were found in the provided time span
  //facebook parameter since <-> until
  case class NotEnoughFound[A: TypeTag : ClassTag](entities: Vector[A])

  //Will be sent if the required minimum or more entities were found
  case class FinishedRetrievingEntities[A: TypeTag](entities: Vector[A])

  case class PartialResult[A: TypeTag](entities: Vector[A])


  private case class NotEnoughRetrieved[A](client: ActorRef,
                                           paging: Option[Paging],
                                           minimum: Int,
                                           count: Int = 0,
                                           entities: Vector[A] = Vector())

  private case class GetEntities[A](client: ActorRef,
                                    path: String,
                                    minimum: Int,
                                    count: Int = 0)

  /**
    * Creates a retrieve entities service
    * @param filter filters out unwanted entities
    * @param mf implicit manifest used for automatic json parsing
    * @tparam T type of the retrieved entities
    * @return props for the created service
    */
  def props[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]): Props =
    Props(new RetrieveEntitiesService[T](filter))
}

class RetrieveEntitiesService[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]) extends FBCommunicationManager {

  /**
    * Entry point of the service, handles the RetrieveEntities(params) message.
    * @return Nothing
    */
  def receive = {
    case RetrieveEntities(params) =>
      val originalSender = sender()
      val path = s"$facebookPath/" + {
        params.query match {
          case Some(q) => q + {
            params.access_token match {
              case Some(a) => s"&access_token=$a"
              case None => ""
            }
          }
          case None => ""

        }
      }
      // The retriever will keep retrieving entities as long as GetEntities messages are sent
      context.become(retrieveEntities())
      self ! GetEntities[T](originalSender, path, params.minimalEntities)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }

  /**
    * Defines the retrieveEntities behavior, in this state the acors retrieves entities as long as GetEntities messages
    * are received. The following messages are handled:
    * - GetEntities(client, path, minimum, count): retrieves new entities for client with path, at least minimum entities
    * must be retrieved, the current number of entities is count
    * - NotEnoughRetrieved(client, paging, minimum, count, entities): states that not enough entities were retrieved up
    * to now, a GetEntities(client, path, minimum, count) message will be sent to self if paging contains enough
    * information to get more entities
    * @return Nothing
    */
  private def retrieveEntities(): Receive = {
    case GetEntities(client, path, minimum, count) =>
      handleGetEntities(client, path, minimum, count)
    case NotEnoughRetrieved(client, paging, minimum, count, entities: Vector[T]) =>
      handleNotEnough(client, paging, minimum, count, entities)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }


  /**
    * Handles a GetEntities request by sending a request to Facebook and parsing the content. If enough entities
    * are retrieved a termination message is sent ot the client otherwise a prtial result is sent to the client and a
    * NotEnoughRetrieved message is sent to self
    * @param client original requester
    * @param path request path
    * @param minimum minimum number of entities to retrieve, 0 means no minimum
    * @param count current number of entities retrieved
    */
  private def handleGetEntities(client: ActorRef, path: String, minimum: Int, count: Int = 0): Unit = {
    log.debug(s"Retriever path: $path")
    val responseF = pipelineRawJson(Get(path))
    responseF.onComplete {
      case Success(r) =>
        r.status match {
          case OK =>
            val json = parse(r.entity.asString)
            val root = rootFromJson(json)

            val single = singleFromRoot(root, json)
            val newEntities: Vector[T] = single match {
              case None =>
                filter(root.data.getOrElse(List[T]()).toVector)
              case Some(entity) =>
                filter(Vector(entity))
            }

            val newCount = count + newEntities.length
            if (newCount < minimum || minimum == 0) {
              self ! NotEnoughRetrieved(client, root.paging, minimum, newCount, newEntities)
            } else {
              client ! FinishedRetrievingEntities[T](newEntities)
            }
          case BadRequest =>
            log.error(s"Facebook gave bad request for path: $path")
            client ! NotEnoughFound(Vector[T]())
          case _ =>
            client ! NotEnoughFound(Vector[T]())
            log.error(s"Can't retrieve entities due to unknown error ${r.status}")
        }
      case Failure(error) =>
        log.error(s"Facebook didn't respond \npath:$path\n  ${error.toString}")
        client ! NotEnoughFound(Vector[T]())
        context.become(receive)

      case _ =>
        log.error(s"Facebook didn't respond \npath:$path.")
        client ! NotEnoughFound(Vector[T]())
        context.become(receive)
    }
  }

  /**
    * Handles NotEnoughRetrieved message. If paging provides enough information a GetEntities request is sent to self,
    * otherwise if the minimum is not met sends a failure message to the client, finally if the minimum is met, a
    * termination message is sent to the client.
    * @param client original requester
    * @param paging paging information (see Facebook graph api documentation)
    * @param minimum minimum number of information to retrieve, 0 means no minimum
    * @param count current number of entities retrieved
    * @param entities the retrieved entities with last request
    */
  private def handleNotEnough(client: ActorRef, paging: Option[Paging], minimum: Int, count: Int = 0,
                              entities: Vector[T]): Unit = {
    paging match {
      case Some(p) => p.next match {
        case Some(next) =>
          self ! GetEntities(client, next, minimum, count)
          client ! PartialResult(entities)
        case None =>
          if (minimum == 0) {
            client ! FinishedRetrievingEntities(entities)
          } else {
            log.info(s"Not enough found end of paging")
            client ! NotEnoughFound(entities)
          }
          context.become(receive)
      }
      case None =>
        if (minimum == 0) {
          client ! FinishedRetrievingEntities(entities)
        } else {
          log.info(s"Not enough found end of paging")
          client ! NotEnoughFound(entities)
        }
        context.become(receive)
    }
  }

  /**
    * Extracts a root object from a json object
    * @param json json object to parse
    * @return the extracted root
    */
  private def rootFromJson(json: JValue): Root[_ <: List[T]] = json.extract[Root[List[T]]] match {
    case Root(None, _, _) =>
      val root = json.extract[Root[T]]
      root.data match {
        case Some(data) => Root(Option(List(data)), root.paging)
        case None => Root(None, root.paging)
      }
    case _@result => result
  }

  /**
    * Extracts a single value from a root object and a json object
    * @param root the root to be examined
    * @param json the json to be parsed
    * @return an optional value which was extracted
    */
  private def singleFromRoot(root: Root[_ <: List[T]], json: JValue): Option[T] = root match {
    case Root(None, _, _) =>
      Try(json.extract[T]) match {
        case Success(j) => Some(j)
        case Failure(e) =>
          log.info(s"$e")
          None
        case _ =>
          None
      }
    case _ => None
  }

}

