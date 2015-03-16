package service.question_generators

import akka.actor.{ActorContext, ActorLogging, Actor}
import entities.Entities.GameQuestion
import entities.Entities.QuestionType._
import entities.Entities.SpecificQuestionType.SpecificQuestionType
import server.domain.RestMessage

import scala.concurrent.ExecutionContextExecutor

/**
 * Created by roger on 20/11/14.
 */

object QuestionGenerator {

  case class CreateQuestion(user_id: String) extends RestMessage
  case class FinishedQuestionCreation(question: GameQuestion)
  case class FailedToCreateQuestion(message: String, questionType: SpecificQuestionType)

}

abstract class QuestionGenerator extends Actor with ActorLogging{
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher
  implicit def actorRefFactory: ActorContext = context

}
