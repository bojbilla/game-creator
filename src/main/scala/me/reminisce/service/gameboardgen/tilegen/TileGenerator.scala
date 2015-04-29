package me.reminisce.service.gameboardgen.tilegen

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{GameQuestion, _}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
import me.reminisce.service.gameboardgen.questiongen._
import me.reminisce.service.gameboardgen.tilegen.TileGenerator._
import reactivemongo.api.DefaultDB

object TileGenerator {
  def props(database: DefaultDB): Props =
    Props(new TileGenerator(database))

  case class CreateMultipleChoiceTile(user_id: String)

  case class CreateTimelineTile(user_id: String)

  case class CreateGeolocationTile(user_id: String)

  case class CreateTile(user_id: String, choices: List[(String, String)])

  case class FinishedTileCreation(user_id: String, tile: Tile)

  case class FailedTileCreation(message: String)

}

class TileGenerator(db: DefaultDB) extends QuestionGenerator {
  var questions = List[GameQuestion]()
  var questionPossibilities: List[SpecificQuestionType] = List()
  var counter = 0
  val limit = 10

  def receive = {

    case CreateTile(user_id, choices) =>
      choices.foreach {
        choice =>
          val generator = createQuestionGenerators(SpecificQuestionType.withName(choice._2))
          generator ! CreateQuestion(user_id, choice._1)
      }
      val client = sender()
      context.become(awaitingQuestions(client, user_id))
  }

  def createQuestionGenerators(questionType: SpecificQuestionType): ActorRef = {
    questionType match {
      case MCWhichPageDidYouLike =>
        context.actorOf(WhichPageDidYouLike.props(db))
      case MCWhoLikedYourPost =>
        context.actorOf(WhoLikedYourPost.props(db))
      case MCWhoMadeThisCommentOnYourPost =>
        context.actorOf(WhoMadeThisCommentOnYourPost.props(db))
      case TLWhenDidYouShareThisPost =>
        context.actorOf(WhenDidYouShareThisPost.props(db))
      case GeoWhichPlaceWereYouAt =>
        context.actorOf(WhichPlaceWereYouAt.props(db))
      case GeoWhatCoordinatesWereYouAt =>
        context.actorOf(WhichCoordinatesWereYouAt.props(db))
      case _ => log.error("Unknown Question Type")
        context.actorOf(WhichPageDidYouLike.props(db))
    }
  }

  def awaitingQuestions(client: ActorRef, user_id: String): Receive = {
    case FinishedQuestionCreation(q) =>
      questions = q :: questions
      sender() ! PoisonPill
      if (questions.length >= 3) {
        val tile = Tile(questions(0), questions(1), questions(2))
        client ! FinishedTileCreation(user_id, tile)
      }

    case FailedToCreateQuestion(message, specificType) =>
      log.error(s"Question generation for tile failed $message for type $specificType")
      sender() ! PoisonPill
      client ! FailedTileCreation(message)

  }
}
