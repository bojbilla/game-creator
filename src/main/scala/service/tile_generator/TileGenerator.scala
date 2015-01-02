package service.tile_generator

import akka.actor.SupervisorStrategy.Stop
import akka.actor.{PoisonPill, ActorRef, Props}
import entities.Entities.QuestionType._
import entities.Entities.QuestionType.QuestionType
import entities.Entities.SpecificQuestionType._
import entities.Entities._
import reactivemongo.api.DefaultDB
import service.question_generators.QuestionGenerator.{FailedToCreateQuestion, FinishedQuestionCreation, CreateQuestion}
import service.question_generators._
import service.tile_generator.TileGenerator._

import scala.util.Random

/**
 * Created by roger on 24/12/14.
 */
object TileGenerator {
  def props(database: DefaultDB): Props =
    Props(new TileGenerator(database))

  case class CreateMultipleChoiceTile(user_id: String)
  case class CreateTimelineTile(user_id: String)
  case class CreateGeolocationTile(user_id: String)

  case class FinishedTileCreation(user_id: String, tile: Tile)
  case class FailedTileCreation(message: String)
}

class TileGenerator(db: DefaultDB) extends QuestionGenerator{
  var questions = List[GameQuestion]()
  var questionPossibilities: List[SpecificQuestionType] = List()
  def receive = {
    case CreateMultipleChoiceTile(user_id) =>
      val client = sender()
      questionPossibilities = Random.shuffle(List(MCWhichPageDidYouLike, MCWhoLikedYourPost, MCWhoMadeThisCommentOnYourPost))
      val actors =
        createQuestionGenerators(questionPossibilities.head) ::
        createQuestionGenerators(questionPossibilities.head) ::
        createQuestionGenerators(questionPossibilities.head) :: Nil
      questionPossibilities = questionPossibilities.tail
      actors foreach { a =>
        a ! CreateQuestion(user_id)
      }
      context.become(awaitingQuestions(client, user_id, MultipleChoice))

    case CreateTimelineTile(user_id) =>
      val client = sender()
      questionPossibilities = Random.shuffle(List(TLWhenDidYouShareThisPost))
      val actors =
        createQuestionGenerators(questionPossibilities.head) ::
          createQuestionGenerators(questionPossibilities.head) ::
          createQuestionGenerators(questionPossibilities.head) :: Nil
      questionPossibilities = questionPossibilities.tail
      actors foreach { a =>
        a ! CreateQuestion(user_id)
      }
      context.become(awaitingQuestions(client, user_id, Timeline))


  }

  def createQuestionGenerators(questionType:  SpecificQuestionType): ActorRef = {
     questionType match {
      case MCWhichPageDidYouLike =>
        context.actorOf(WhichPageDidYouLike.props(db))
      case MCWhoLikedYourPost =>
        context.actorOf(WhoLikedYourPost.props(db))
      case MCWhoMadeThisCommentOnYourPost =>
        context.actorOf(WhoMadeThisCommentOnYourPost.props(db))
      case TLWhenDidYouShareThisPost =>
         context.actorOf(WhenDidYouShareThisPost.props(db))
      case _ => log.error("Unknown Question Type")
        context.actorOf(WhichPageDidYouLike.props(db))
     }
  }

  def awaitingQuestions(client: ActorRef, user_id: String, questionType: QuestionType): Receive = {
    case FinishedQuestionCreation(q) =>
      questions = q :: questions
      sender() ! PoisonPill
      if (questions.length >= 3) {
        val tile = Tile(questionType, questions(0), questions(1), questions(2))
        client ! FinishedTileCreation(user_id, tile)
      }
    case FailedToCreateQuestion(message, specificType) =>
      log.error(s"Question generation for tile failed $message for type $specificType" )
      sender() ! PoisonPill
      questionPossibilities = questionPossibilities.filter(p => p != specificType)
      questionPossibilities match {
        case x :: xs =>
          val actor = createQuestionGenerators(x)
          actor ! CreateQuestion(user_id)
        case Nil =>
          log.error(s"No more possiblities")
          client ! FailedTileCreation(message)
      }

  }
}
