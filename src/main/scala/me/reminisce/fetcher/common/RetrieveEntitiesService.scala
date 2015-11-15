package me.reminisce.fetcher.common

import akka.actor.{ActorRef, Props}
import me.reminisce.fetcher.common.GraphResponses.{Paging, Root}
import me.reminisce.fetcher.common.RetrieveEntitiesService._
import me.reminisce.server.domain.RestMessage
import org.json4s.JsonAST.JValue
import org.json4s.jackson.JsonMethods._
import spray.client.pipelining._
import spray.http.StatusCodes._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success, Try}

object RetrieveEntitiesService {

  //Used for starting to fetch data on facebook
  //  case class RetrieveEntities(parameters: FBParameters)
  case class RetrieveEntities(params: FBSimpleParameters) extends RestMessage

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


  def props[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]): Props =
    Props(new RetrieveEntitiesService[T](filter))


}

class RetrieveEntitiesService[T](filter: (Vector[T]) => Vector[T])(implicit mf: Manifest[T]) extends FBCommunicationManager {

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
      context.become(retrieveEntities())
      self ! GetEntities[T](originalSender, path, params.minimalEntities)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }

  def retrieveEntities(): Receive = {
    case GetEntities(client, path, minimum, count) =>
      handleGetEntities(client, path, minimum, count)
    case NotEnoughRetrieved(client, paging, minimum, count, entities: Vector[T]) =>
      handleNotEnough(client, paging, minimum, count, entities)
    case any =>
      log.error(s"RetrieveEntitiesService received an unexpected message : $any.")
  }


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
          case BadRequest => log.error(s"Facebook gave bad request for path: $path")
            client ! NotEnoughFound(Vector[T]())
          case _ =>
            client ! NotEnoughFound(Vector[T]())
            log.error(s"Can't retrieve entities due to unknown error ${r.status}")
        }
      case Failure(error) =>
        log.error(s"Facebook didn't respond \npath:$path\n  ${error.toString}")
        client ! NotEnoughFound(Vector[T]())
        context.become(receive)
    }
  }


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

  private def rootFromJson(json: JValue): Root[_ <: List[T]] = json.extract[Root[List[T]]] match {
    case Root(None, _, _) =>
      val root = json.extract[Root[T]]
      root.data match {
        case Some(data) => Root(Option(List(data)), root.paging)
        case None => Root(None, root.paging)
      }
    case _@result => result
  }

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

