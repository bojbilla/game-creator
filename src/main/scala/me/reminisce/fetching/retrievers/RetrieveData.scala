package me.reminisce.fetching.retrievers

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.fetching.config.FacebookServiceConfig
import org.json4s.DefaultFormats
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse}

import scala.concurrent.{ExecutionContextExecutor, Future}

/**
  * Basic abstract retriever actor class
  * Defines a few routines and values useful for the child classes
  */
abstract class RetrieveData extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  implicit val formats = DefaultFormats

  def defaultFilter[T](entities: Vector[T]): Vector[T] = {
    for {
      e <- entities
    } yield e
  }

  def facebookPath = s"${FacebookServiceConfig.facebookHostAddress}"

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )

}

