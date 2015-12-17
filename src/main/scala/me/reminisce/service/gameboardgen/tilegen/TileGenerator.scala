package me.reminisce.service.gameboardgen.tilegen

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
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
  case class CreateTile(userId: String, choices: List[(QuestionKind, DataType, List[(String, String)])], tpe: QuestionKind = Misc)

  case class FinishedTileCreation(userId: String, tile: Tile)

  case class FailedTileCreation(message: String)

}

class TileGenerator(db: DefaultDB) extends QuestionGenerator {
  val limit = 10

  def receive = {

    case CreateTile(userId, choices, tpe) =>
      val client = sender()
      choices.foreach {
        case (questionKind, dataType, itemIdTypes) =>
          val generator = questionInference((questionKind, dataType, itemIdTypes))
          if (questionKind == Order) {
            generator ! CreateQuestionWithMultipleItems(userId, itemIdTypes.map { case (itemId, itemType) => itemId })
          } else {
            itemIdTypes.headOption match {
              case Some((itemId, itemType)) =>
                generator ! CreateQuestion(userId, itemId)
              case None =>
                client ! FailedTileCreation(s"Choice had no itemId.")
            }
          }
      }
      context.become(awaitingQuestions(client, userId, tpe, List[GameQuestion]()))
    case any =>
      log.error(s"Tile generator received an unsupported message: $any.")
  }

  private def questionInference(kindTypeWithItem: (QuestionKind, DataType, List[(String, String)])): ActorRef = kindTypeWithItem match {
    case (kind, tpe, item) =>
      kind match {
        case Order =>
          tpe match {
            case LikeNumber =>
              item.headOption match {
                case Some((_, "Post")) =>
                  context.actorOf(OrderByPostLikesNumber.props(db))
                case _ =>
                  context.actorOf(OrderByPageLikes.props(db))
              }
            case PostCommentsNumber =>
              context.actorOf(OrderByPostCommentsNumber.props(db))
            case Time =>
              item.headOption match {
                case Some((_, "Post")) =>
                  context.actorOf(OrderByPostTime.props(db))
                case _ =>
                  context.actorOf(OrderByPageLikeTime.props(db))
              }
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
          item.headOption match {
            case Some((_, "Post")) =>
              context.actorOf(WhenDidYouShareThisPost.props(db))
            case _ =>
              context.actorOf(WhenDidYouLikeThisPage.props(db))
          }
        case Geolocation =>
          context.actorOf(WhichCoordinatesWereYouAt.props(db))
      }
  }

  private def awaitingQuestions(client: ActorRef, userId: String, qType: QuestionKind, questions: List[GameQuestion]): Receive = {
    case FinishedQuestionCreation(q) =>
      val newQuestions = q :: questions
      sender() ! PoisonPill
      newQuestions match {
        case q1 :: q2 :: q3 :: qis =>
          val tile = Tile(qType, q1, q2, q3)
          client ! FinishedTileCreation(userId, tile)
        case _ =>
          context.become(awaitingQuestions(client, userId, qType, newQuestions))
      }

    case MongoDBError(message) =>
      log.error(s"Question generation for tile failed, mongodb error : $message.")
      sender() ! PoisonPill
      client ! FailedTileCreation(s"MongoDBError: $message.")

    case NotEnoughData(message) =>
      log.error(s"Not enough data : $message")
      sender() ! PoisonPill
      client ! FailedTileCreation(s"Not enough data : $message")

    case any =>
      log.error(s"Tile generator received an unknown messgae : $any.")
  }
}
