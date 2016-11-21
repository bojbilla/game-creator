package me.reminisce.gameboard.board

import akka.actor.{ActorRef, PoisonPill, Props}
import me.reminisce.analysis.DataTypes._
import me.reminisce.gameboard.board.GameboardEntities.{GameQuestion, _}
import me.reminisce.gameboard.board.TileGenerator.{CreateTile, FailedTileCreation, FinishedTileCreation}
import me.reminisce.gameboard.questions.QuestionGenerator._
import me.reminisce.gameboard.questions._
import reactivemongo.api.DefaultDB

/**
  * Factory for [[me.reminisce.gameboard.board.TileGenerator]] and case classes for message passing
  */
object TileGenerator {
  /**
    * Creates a tile generator actor
    *
    * @param database the database from which the data is read
    * @return props for the created tile generator
    */
  def props(database: DefaultDB): Props =
  Props(new TileGenerator(database))

  // The List[(String, ItemType)] is for (itemId, itemType)
  case class CreateTile(userId: String, choices: List[(QuestionKind, DataType, List[(String, ItemType)])], tpe: QuestionKind = Misc)

  case class FinishedTileCreation(userId: String, tile: Tile)

  case class FailedTileCreation(message: String)

}

class TileGenerator(db: DefaultDB) extends QuestionGenerator {
  /**
    * This actor's entry point, handles the CreateTile(userId, choices, tpe) message. For each choice it instantiates the
    * appropriate question generator and requests a question generation.
    *
    * @return Nothing
    */
  def receive = {

    case CreateTile(userId, choices, tpe) =>
      val client = sender()
      choices.foreach {
        case (questionKind, dataType, itemIdTypes) =>
          val maybeGenerator = questionInference((questionKind, dataType, itemIdTypes))
          maybeGenerator match {
            case Some(generator) =>
              if (questionKind == Order) {
                generator ! CreateQuestionWithMultipleItems(userId, itemIdTypes.map {
                  case (itemId, itemType) => itemId
                })
              } else {
                itemIdTypes.headOption match {
                  case Some((itemId, itemType)) =>
                    generator ! CreateQuestion(userId, itemId)
                  case None =>
                    client ! FailedTileCreation(s"Choice had no itemId.")
                }
              }
            case None =>
              log.error(s"Generator could not be inferred from kind $questionKind and type $dataType for user $userId.")
          }
      }
      context.become(awaitingQuestions(client, userId, tpe, List[GameQuestion]()))
    case any =>
      log.error(s"Tile generator received an unsupported message: $any.")
  }

  /**
    * Determines which question has to be generated based on the QuestionKind and the DataType
    *
    * @param kindTypeWithItem a tuple representing a question
    * @return a question generator
    */
  private def questionInference(kindTypeWithItem: (QuestionKind, DataType, List[(String, ItemType)])): Option[ActorRef] = kindTypeWithItem match {
    case (kind, tpe, item) =>
      kind match {
        case Order =>
          tpe match {
            case PostReactionNumber =>
              Some(context.actorOf(OrderByPostReactionsNumber.props(db)))
            case PageLikeNumber =>
              Some(context.actorOf(OrderByPageLikes.props(db)))
            case PostCommentsNumber =>
              Some(context.actorOf(OrderByPostCommentsNumber.props(db)))
            case Time =>
              item.headOption match {
                case Some((_, PostType)) =>
                  Some(context.actorOf(OrderByPostTime.props(db)))
                case _ =>
                  Some(context.actorOf(OrderByPageLikeTime.props(db)))
              }
            case _ =>
              None
          }
        case MultipleChoice =>
          tpe match {
            case PageWhichLiked =>
              Some(context.actorOf(WhichPageDidYouLike.props(db)))
            case reaction: ReactionType =>
              Some(context.actorOf(WhoReactedToYourPost.props(db, reaction)))
            case _ =>
              None
          }
        case Timeline =>
          item.headOption match {
            case Some((_, PostType)) =>
              Some(context.actorOf(WhenDidYouShareThisPost.props(db)))
            case _ =>
              Some(context.actorOf(WhenDidYouLikeThisPage.props(db)))
          }
        case Geolocation =>
          Some(context.actorOf(WhichCoordinatesWereYouAt.props(db)))

        case _ =>
          None
      }
  }

  /**
    * Waits on feedback from the generator workers. The parameters hold this actor's state Handles the following messages:
    * - FinishedQuestionCreation(q): a generator finished creating the question. Add the question to the questions list,
    * checks if three questions were created and if so, reports back to client
    * - MongoDBError(message): an error occurred while contacting the database, report to the client
    * - NotEnoughData(message): there was not enough data, report to the client
    *
    * @param client    tile requester
    * @param userId    user for which the questions are created
    * @param qType     tile question type (can be Misc)
    * @param questions already generated questions
    * @return Nothing
    */
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
