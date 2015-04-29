package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities.GameQuestion
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType.SpecificQuestionType

import scala.concurrent.ExecutionContextExecutor

object QuestionGenerator {

  case class CreateQuestion(user_id: String) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class FailedToCreateQuestion(message: String, questionType: SpecificQuestionType) extends RestMessage

}

abstract class QuestionGenerator extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

}
