package me.reminisce.fetcher.common

import akka.actor.{Actor, ActorLogging}
import me.reminisce.fetcher.FacebookServiceConfig
import org.json4s.DefaultFormats
import spray.client.pipelining._
import spray.http.HttpHeaders.Accept
import spray.http.MediaTypes._
import spray.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future


abstract class FBCommunicationManager extends Actor with ActorLogging {
  implicit def dispatcher = context.dispatcher

  implicit def actorRefFactory = context

  implicit val formats = DefaultFormats

  def defaultFilter[T](entities: Vector[T]): Vector[T] = {
    for {
      e <- entities
    } yield e
  }

  def facebookPath = s"${FacebookServiceConfig.facebookHostAddress}/${FacebookServiceConfig.apiVersion}"

  implicit val pipelineRawJson: HttpRequest => Future[HttpResponse] = (
    addHeader(Accept(`application/json`))
      ~> sendReceive
    )


}
