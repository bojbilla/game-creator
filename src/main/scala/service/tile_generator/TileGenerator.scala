package service.tile_generator

import akka.actor.{ActorRef, PoisonPill, Props}
import entities.Entities.QuestionType.{QuestionType, _}
import entities.Entities.SpecificQuestionType._
import entities.Entities.{GameQuestion, _}
import reactivemongo.api.DefaultDB
import service.question_generators.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
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
  var counter = 0
  val limit = 10

  def receive = {
    case CreateMultipleChoiceTile(user_id) =>
      val client = sender()
      questionPossibilities = Random.shuffle(List(MCWhichPageDidYouLike, MCWhoLikedYourPost, MCWhoMadeThisCommentOnYourPost))
      val actors =
        createQuestionGenerators(questionPossibilities.head) ::
        createQuestionGenerators(questionPossibilities.head) ::
        createQuestionGenerators(questionPossibilities.head) :: Nil
      questionPossibilities = questionPossibilities
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
      questionPossibilities = questionPossibilities
      actors foreach { a =>
        a ! CreateQuestion(user_id)
      }
      context.become(awaitingQuestions(client, user_id, Timeline))


  }

  def createQuestionGenerators(questionType:  SpecificQuestionType): ActorRef = {
     questionType match {
      case MCWhichPageDidYouLike =>
        log.info(s"Trying to create question WhichPageDidYouLike")
        context.actorOf(WhichPageDidYouLike.props(db))
      case MCWhoLikedYourPost =>
        log.info(s"Trying to create question WhoLikedYourPost")
        context.actorOf(WhoLikedYourPost.props(db))
      case MCWhoMadeThisCommentOnYourPost =>
        log.info(s"Trying to create question WhoMadeThisCommentOnYourPost")
        context.actorOf(WhoMadeThisCommentOnYourPost.props(db))
      case TLWhenDidYouShareThisPost =>
        log.info(s"Trying to create question WhenDidYouShareThisPost")
        context.actorOf(WhenDidYouShareThisPost.props(db))
      case _ => log.error("Unknown Question Type")
        log.error(s"Trying to create question Unknown Question Type")
        context.actorOf(WhichPageDidYouLike.props(db))
     }
  }

  def awaitingQuestions(client: ActorRef, user_id: String, questionType: QuestionType): Receive = {
    case FinishedQuestionCreation(q) =>
      log.info(s"Created question of type $questionType for user $user_id")
      if (questions.filter(p => p.id == q.id).isEmpty) {
        questions = q :: questions
        log.info(s"Added question: $questionType for user $user_id having now ${questions.length}")
        sender() ! PoisonPill
        if (questions.length >= 3) {
          log.info(s"Creating Tile: $questionType for user $user_id")
          val tile = Tile(questionType, questions(0), questions(1), questions(2))
          client ! FinishedTileCreation(user_id, tile)
        }
      } else {
        sender() ! PoisonPill
        counter = counter + 1
        if (counter >= limit){
          log.info(s"Trying another questiontype for user: $user_id")
          questionPossibilities = questionPossibilities.tail
        }
        questionPossibilities match {
          case x :: xs =>
            val actor = createQuestionGenerators(x)
            actor ! CreateQuestion(user_id)
          case Nil =>
            log.error(s"No more possiblities for user $user_id")
            client ! FailedTileCreation("Not enough content (questions too similar)")
        }

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
          log.error(s"No more possiblities for user $user_id")
          client ! FailedTileCreation(message)
      }

  }
}
