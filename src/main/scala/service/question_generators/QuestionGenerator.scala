package service.question_generators

import akka.actor.{ActorContext, ActorLogging, Actor}

import scala.concurrent.ExecutionContextExecutor

/**
 * Created by roger on 20/11/14.
 */
abstract class QuestionGenerator extends Actor with ActorLogging{
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher
  implicit def actorRefFactory: ActorContext = context

}
