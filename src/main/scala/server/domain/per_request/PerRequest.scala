package routing

/**
 * Created by Aranir on 23/10/14.
 */

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{OneForOneStrategy, _}
import org.json4s.DefaultFormats
import org.json4s.jackson
import routing.PerRequest.{WithProps, WithActorRef}
import server.domain.{Domain, RestMessage}
import server.json_serializer.GameCreatorFormatter
import service.question_generators.QuestionGenerator.FinishedQuestionCreation
import spray.http.StatusCode
import spray.http.StatusCode
import spray.http.StatusCodes._
import spray.http.StatusCodes._
import spray.httpx.Json4sSupport
import spray.httpx.Json4sSupport
import spray.routing.RequestContext
import spray.routing.RequestContext
import scala.concurrent.duration._



trait PerRequest extends Actor with Json4sSupport with ActorLogging with GameCreatorFormatter{

  import context._

  def r: RequestContext
  def target: ActorRef
  def message: RestMessage
  setReceiveTimeout(10 seconds)
  target ! message

  def receive = {
    case res: RestMessage => complete(OK, res)
    case FinishedQuestionCreation(q) => complete(OK, q)
    case error: Domain.Error => complete(NotFound, error)
    case tooMany: Domain.TooManyRequests => complete(TooManyRequests, tooMany)
    case v: Domain.Validation    => complete(BadRequest, v)
    case ReceiveTimeout   => complete(GatewayTimeout, Domain.Error("Request timeout"))
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

object PerRequest {
  case class WithActorRef(r: RequestContext, target: ActorRef, message: RestMessage) extends PerRequest

  case class WithProps(r: RequestContext, props: Props, message: RestMessage) extends PerRequest {
    lazy val target = context.actorOf(props)
  }
}

trait PerRequestCreator {
  this: Actor =>

  def perRequest(r: RequestContext, target: ActorRef, message: RestMessage) =
    context.actorOf(Props(new WithActorRef(r, target, message)))

  def perRequest(r: RequestContext, props: Props, message: RestMessage) =
    context.actorOf(Props(new WithProps(r, props, message)))
}