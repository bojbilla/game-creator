package me.reminisce.service.gameboardgen.tilegen

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{GameQuestion, _}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import me.reminisce.service.gameboardgen.questiongen._
import me.reminisce.service.gameboardgen.tilegen.TileGenerator._
import me.reminisce.service.stats.StatsDataTypes._
import reactivemongo.api.DefaultDB

object TileGenerator {
  def props(database: DefaultDB): Props =
    Props(new TileGenerator(database))

  // The List[(String, String)] is for (itemId, itemType)
  case class CreateTile(userId: String, choices: List[(QuestionKind, DataType, List[(String, String)])], `type`: QuestionKind = Misc)

  case class FinishedTileCreation(userId: String, tile: Tile)

  case class FailedTileCreation(message: String)

}

class TileGenerator(db: DefaultDB) extends QuestionGenerator {
  var questions = List[GameQuestion]()
  var questionPossibilities: List[SpecificQuestionType] = List()
  var counter = 0
  val limit = 10

  def receive = {

    case CreateTile(userId, choices, tpe) =>
      choices.foreach {
        choice =>
          val generator = questionInference(choice)
          if (choice._1 == Order) {
            generator ! CreateQuestionWithMultipleItems(userId, choice._3.map(_._1))
          } else {
            generator ! CreateQuestion(userId, choice._3.head._1)
          }
      }
      val client = sender()
      context.become(awaitingQuestions(client, userId, tpe))
  }

  def questionInference(kindTypeWithItem: (QuestionKind, DataType, List[(String, String)])): ActorRef = {
    val (kind, tpe, item) = kindTypeWithItem
    kind match {
      case Order =>
        tpe match {
          case LikeNumber =>
            if (item.head._2 == "Post")
              context.actorOf(OrderByPostLikesNumber.props(db))
            else
              context.actorOf(OrderByPageLikes.props(db))
          case PostCommentsNumber =>
            context.actorOf(OrderByPostCommentsNumber.props(db))
          case Time =>
            if (item.head._2 == "Post")
              context.actorOf(OrderByPostTime.props(db))
            else
              context.actorOf(OrderByPageLikeTime.props(db))
        }
      case MultipleChoice =>
        tpe match {
          case PostWhoLiked =>
            context.actorOf(WhoLikedYourPost.props(db))
          case PostWhoCommented =>
            context.actorOf(WhoMadeThisCommentOnYourPost.props(db))
          case PageWhichLiked =>
            context.actorOf(WhichPageDidYouLike.props(db))
        }
      case Timeline =>
        item.head._2 match {
          case "Post" =>
            context.actorOf(WhenDidYouShareThisPost.props(db))
          case "Page" =>
            context.actorOf(WhenDidYouLikeThisPage.props(db))
        }
      case Geolocation =>
        context.actorOf(WhichCoordinatesWereYouAt.props(db))
    }
  }

  def awaitingQuestions(client: ActorRef, userId: String, `type`: QuestionKind): Receive = {
    case FinishedQuestionCreation(q) =>
      questions = q :: questions
      sender() ! PoisonPill
      if (questions.length >= 3) {
        val tile = Tile(`type`, questions(0), questions(1), questions(2))
        client ! FinishedTileCreation(userId, tile)
      }

    case MongoDBError(message) =>
      log.error(s"Question generation for tile failed, mongodb error : $message.")
      sender() ! PoisonPill
      client ! FailedTileCreation(s"MongoDBError: $message.")

    case NotEnoughData(message) =>
      log.error(s"Not enough data : $message")
      sender() ! PoisonPill
      client ! FailedTileCreation(s"Not enough data : $message")
  }
}
