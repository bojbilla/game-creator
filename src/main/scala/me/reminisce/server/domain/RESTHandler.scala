package me.reminisce.server.domain

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{OneForOneStrategy, _}
import me.reminisce.gameboard.questions.QuestionGenerator.FinishedQuestionCreation
import me.reminisce.server.domain.Domain.{ActionForbidden, Error}
import me.reminisce.server.domain.RESTHandler.{WithActorRef, WithProps}
import me.reminisce.server.jsonserializer.GameCreatorFormatter
import spray.http.StatusCode
import spray.http.StatusCodes._
import spray.httpx.Json4sSupport
import spray.routing.RequestContext

import scala.concurrent.duration._


trait RESTHandler extends Actor with Json4sSupport with ActorLogging with GameCreatorFormatter {

  import context._

  def r: RequestContext

  def target: ActorRef

  def message: RestMessage

  setReceiveTimeout(10.seconds)
  target ! message

  def receive = {
    case FinishedQuestionCreation(q) => complete(OK, q)
    case error: Error => complete(NotFound, error)
    case tooMany: Domain.TooManyRequests => complete(TooManyRequests, tooMany)
    case v: Domain.Validation => complete(BadRequest, v)
    case ReceiveTimeout => complete(GatewayTimeout, Domain.Error("Request timeout"))
    case graphUnreachable: Domain.GraphAPIUnreachable => complete(GatewayTimeout, graphUnreachable)
    case graphInvalidToken: Domain.GraphAPIInvalidToken => complete(Unauthorized, graphInvalidToken)
    case internalError: Domain.InternalError => complete(InternalServerError, internalError)
    case forbidden: ActionForbidden => complete(Forbidden, forbidden)
    case res: RestMessage => complete(OK, res)
    case x => log.info("Per request received strange message " + x)
  }

  def complete[T <: AnyRef](status: StatusCode, obj: T) = {
    r.complete(status, obj)
    stop(self)
  }

  override val supervisorStrategy =
    OneForOneStrategy() {
      case e =>
        complete(InternalServerError, Domain.Error(e.getMessage))
        Stop
    }
}

object RESTHandler {

  case class WithActorRef(r: RequestContext, target: ActorRef, message: RestMessage) extends RESTHandler

  case class WithProps(r: RequestContext, props: Props, message: RestMessage) extends RESTHandler {
    lazy val target = context.actorOf(props)
  }

}

trait RESTHandlerCreator {
  this: Actor =>

  def perRequest(r: RequestContext, target: ActorRef, message: RestMessage) =
    context.actorOf(Props(new WithActorRef(r, target, message)))

  def perRequest(r: RequestContext, props: Props, message: RestMessage) =
    context.actorOf(Props(new WithProps(r, props, message)))
}