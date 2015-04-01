package crawler.common


import akka.actor.{ActorRef, Props}
import crawler.common.GraphResponses.{Paging, Root}
import crawler.common.RetrieveEntitiesService._
import org.json4s.jackson.JsonMethods._
import server.domain.RestMessage
import spray.client.pipelining._
import spray.http.StatusCodes._

import scala.reflect.ClassTag
import scala.reflect.runtime.universe._
import scala.util.{Failure, Success}

object RetrieveEntitiesService{

  //Used for starting to crawl facebook
  //  case class RetrieveEntities(parameters: FBParameters)
  case class RetrieveEntities(params: FBSimpleParameters) extends RestMessage

  //Will be sent if not enough entities were found in the provided time span
  //facebook parameter since <-> until
  case class NotEnoughFound[A: TypeTag: ClassTag](entities: Vector[A])

  //Will be sent if the required minimum or more entities were found

  case class FinishedRetrievingEntities[A: TypeTag](entities:Vector[A]) {
    val tpe = typeOf[A]
  }


  private case class NotEnoughRetrieved[A](client:ActorRef,
                                           paging: Option[Paging],
                                           minimum: Int,
                                           entities: Vector[A] = Vector())

  private case class GetEntities[A](client: ActorRef,
                                    path: String,
                                    minimal: Int,
                                    entities: Vector[A] = Vector.empty[A])


  def props[T](filter:(Vector[T]) => Vector[T])(implicit mf: Manifest[T]): Props =
    Props(new RetrieveEntitiesService[T](filter))


}

class RetrieveEntitiesService[T](filter:(Vector[T]) => Vector[T])(implicit mf: Manifest[T]) extends FBCommunicationManager{
  def receive = {
    case RetrieveEntities(params) =>
      val originalSender = sender()
      val path = s"$facebookPath/v2.2/" + {
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
  }

  def retrieveEntities(): Receive = {
    case GetEntities(client, path, minimum, entities: Vector[T])=>
      log.info(s"Retriever path: $path")
      val responseF = pipelineRawJson(Get(path))
      responseF.onComplete {
        case Success(r) =>
          r.status match {
            case OK =>
              val body = r.entity.asString
              val json = parse(r.entity.asString)
              val root = json.extract[Root[List[T]]] match {
                case Root(None, _, _) =>
                  val root = json.extract[Root[T]]
                  root.data match {
                    case Some(data) => Root(Option(List(data)), root.paging)
                    case None => Root(None, root.paging)
                  }
                case _@result => result
              }

              val single = root match {
                case Root(None, _, _) =>
                  try {
                    Some(json.extract[T])
                  } catch {
                    case e: Exception =>
                      None
                  }
                case _ => None
              }
              var newEntities: Vector[T] = Vector.empty

              //As facebooks response isn't well structured, there are multiple ways the a potential
              //usable object can be constructed
              single match {
                case None =>
                  newEntities = filter(root.data.getOrElse(List[T]()).toVector)
                case Some(entity) =>
                  newEntities = filter(Vector(entity))
              }

              val foundEntities = entities ++ newEntities
              if (foundEntities.length < minimum || minimum == 0) {
                self ! NotEnoughRetrieved(client, root.paging, minimum, foundEntities)
              } else {
                client ! FinishedRetrievingEntities[T](foundEntities)
              }
            case BadRequest => log.error(s"Facebook gave bad request for path: $path")
              client ! NotEnoughFound(entities)
            case _ =>
              client ! NotEnoughFound(entities)
              log.error(s"Can't retrieve entities due to unknown error ${r.status}")
          }
        case Failure(error) =>
          log.error(s"Facebook didn't respond \npath:$path\n  ${error.toString}")
          client ! NotEnoughFound(entities)
          context.become(receive)
      }
    case NotEnoughRetrieved(client, paging, minimum, entities) =>
      paging match {
        case Some(p) => p.next match {
          case Some(next) => self ! GetEntities(client, next, minimum, entities)
          case None =>
            if (minimum == 0){
              client ! FinishedRetrievingEntities(entities)
            } else {
              log.info(s"Not enough found end of paging ${entities.length}")
              client ! NotEnoughFound(entities)
            }
            context.become(receive)
        }
        case None =>
          if (minimum == 0){
            client ! FinishedRetrievingEntities(entities)
          } else {
            log.info(s"Not enough found end of paging${entities.length}")
            client ! NotEnoughFound(entities)
          }
          context.become(receive)
      }
  }

}

