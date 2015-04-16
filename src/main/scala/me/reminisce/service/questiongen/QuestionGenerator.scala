package me.reminisce.service.questiongen

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.entities.Entities.GameQuestion
import me.reminisce.entities.Entities.SpecificQuestionType.SpecificQuestionType
import me.reminisce.server.domain.RestMessage

import scala.concurrent.ExecutionContextExecutor

/**
 * Created by roger on 20/11/14.
 */

object QuestionGenerator {

  case class CreateQuestion(user_id: String) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class FailedToCreateQuestion(message: String, questionType: SpecificQuestionType) extends RestMessage

}

abstract class QuestionGenerator extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

}
