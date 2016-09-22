package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import org.joda.time.DateTime
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

/**
  * Factory for [[me.reminisce.gameboard.questions.OrderByPageLikeTime]]
  */
object OrderByPageLikeTime {

  /**
    * Creates an OrderByPageLikeTime question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new OrderByPageLikeTime(database))
}

/**
  * OrderByPageLikeTime question generator
  * @param db database from which to take the data
  */
class OrderByPageLikeTime(db: DefaultDB) extends OrderQuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val pageLikesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      (for {
        pageLikes <- fetchLikedPages(pageLikesCollection, userId, Some(itemIds))
        pages <- fetchPages(pagesCollection, itemIds)
      } yield {
        if (pageLikes.length >= itemsToOrder && pages.length >= itemsToOrder) {
          FinishedQuestionCreation(generateQuestion(userId, pages, pageLikes))
        } else {
          NotEnoughData(s"Not enough pages or page-likes.")
        }
      }) onComplete {
        case Success(message) =>
          client ! message
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
        case any =>
          client ! MongoDBError(s"$any")
      }

    case any =>
      log.error(s"OrderByPageLikeTime received an unknown message : $any.")
  }

  /**
    * Generate a question
    * @param userId user for which the question is meant
    * @param pages pages choice
    * @param pageLikes page likes corresponding to the pages
    * @return an order question
    */
  private def generateQuestion(userId: String, pages: List[FBPage], pageLikes: List[FBPageLike]): OrderQuestion = {
    val timedPages = pages.map {
      p =>
        pageLikes.find(pl => pl.pageId == p.pageId) match {
          case Some(pageLike) =>
            (p, pageLike.likeTime)
          case None =>
            (p, DateTime.now)
        }
    }
    val orderedTimedPages = timedPages.take(itemsToOrder).sortBy { case (page, likedTime) => likedTime.getMillis }
    val ordered = orderedTimedPages.map { case (page, likedTime) => subjectFromPage(page) }
    OrderQuestionGenerator.generateSubjectsWithId(ordered) match {
      case (subjectsWithId, answer) =>
        OrderQuestion(userId, Order, ORDPageLikeTime, None, subjectsWithId, answer)
    }
  }

}
