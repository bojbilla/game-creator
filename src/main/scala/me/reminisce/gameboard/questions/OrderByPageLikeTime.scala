package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.database.{MongoDBEntities, MongoDatabaseService}
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import org.joda.time.DateTime
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}


object OrderByPageLikeTime {

  def props(database: DefaultDB): Props =
    Props(new OrderByPageLikeTime(database))
}

class OrderByPageLikeTime(db: DefaultDB) extends OrderQuestionGenerator {
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
