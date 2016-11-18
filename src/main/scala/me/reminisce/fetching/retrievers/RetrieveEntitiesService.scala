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
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

/**
  * Factory for [[me.reminisce.fetching.retrievers.RetrieveEntitiesService]] and case classes for message passing
  */
object RetrieveEntitiesService {

  case class RetrieveEntities(params: FBParameters) extends RestMessage

  case class RetrieveError(message: String = "")

  case class FinishedRetrievingEntities[A: TypeTag](entities: Vector[A])

  case class PartialResult[A: TypeTag](entities: Vector[A])

  private case class GetEntities[A](client: ActorRef,
                                    path: String)

  /**
    * Creates a retrieve entities service
    *
    * @param filter filters out unwanted entities
    * @param mf     implicit manifest used for automatic json parsing
    * @tparam T type of the retrieved entities
    * @return props for the created service
    */
  def props[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]): Props =
  Props(new RetrieveEntitiesService[T](filter))
}

class RetrieveEntitiesService[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]) extends FBCommunicationManager {

  /**
    * Entry point of the service, handles the RetrieveEntities(params) message.
    *
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
      context.become(retrieveEntities(path))
      self ! GetEntities[T](originalSender, path)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }

  /**
    * Defines the retrieveEntities behavior, in this state the acors retrieves entities as long as GetEntities messages
    * are received. The following messages are handled:
    * - GetEntities(client, path, minimum, count): retrieves new entities for client with path, at least minimum entities
    * must be retrieved, the current number of entities is count
    *
    * @param originalPath original path, used when after is provided but not next in paging
    * @return Nothing
    */
  private def retrieveEntities(originalPath: String): Receive = {
    case GetEntities(client, path) =>
      handleGetEntities(client, originalPath, path)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }


  /**
    * Handles a GetEntities request by sending a request to Facebook and parsing the content. If enough entities
    * are retrieved a termination message is sent ot the client otherwise a prtial result is sent to the client and a
    * NotEnoughRetrieved message is sent to self
    *
    * @param client       original requester
    * @param originalPath original path, used when after is provided but not next in paging
    * @param path         request path
    */
  private def handleGetEntities(client: ActorRef, originalPath: String, path: String): Unit = {
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

            handlePaging(client, root.paging, newEntities, originalPath)

          case BadRequest =>
            client ! RetrieveError(s"Facebook gave bad request for path: $path")
          case _ =>
            client ! RetrieveError(s"Can't retrieve entities due to unknown error ${r.status}")
        }
      case Failure(error) =>
        client ! RetrieveError(s"Facebook didn't respond, path:$path,  ${error.toString}")

      case _ =>
        client ! RetrieveError(s"Facebook didn't respond, path:$path.")
    }
  }

  /**
    * Looks into paging in order to continue retrieving more entities
    *
    * @param client       original requester
    * @param maybePaging  paging information (see Facebook graph api documentation)
    * @param entities     the retrieved entities with last request
    * @param originalPath original path, used when after is provided but not next in paging
    */
  private def handlePaging(client: ActorRef, maybePaging: Option[Paging], entities: Vector[T], originalPath: String): Unit = {
    val maybeNext: Option[String] = maybePaging.flatMap(_.next)
    maybeNext match {
      case Some(next) =>
        client ! PartialResult(entities)
        handleGetEntities(client, originalPath, next)
      case None =>
        val maybeAfter = maybePaging.flatMap(_.cursors.flatMap(_.after))
        maybeAfter match {
          case Some(after) =>
            client ! PartialResult(entities)
            handleGetEntities(client, originalPath, s"$originalPath&after=$after")
          case None =>
            client ! FinishedRetrievingEntities(entities)
        }
    }
  }

  /**
    * Extracts a root object from a json object
    *
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
    *
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

