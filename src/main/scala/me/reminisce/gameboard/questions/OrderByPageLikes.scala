package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection


object OrderByPageLikes {

  def props(database: DefaultDB): Props =
    Props(new OrderByPageLikes(database))
}

class OrderByPageLikes(db: DefaultDB) extends OrderQuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      fetchPages(pagesCollection, itemIds, client) {
        pageList =>
          if (pageList.length < itemsToOrder) {
            client ! NotEnoughData(s"Not enough pages in list.")
          } else {
            val ordered = pageList.take(itemsToOrder).sortBy(_.likesNumber).map(subjectFromPage)
            OrderQuestionGenerator.generateSubjectsWithId(ordered) match {
              case (subjectsWithId, answer) =>
                val gameQuestion = OrderQuestion(userId, Order, ORDPageLikes, None, subjectsWithId, answer)
                client ! FinishedQuestionCreation(gameQuestion)
            }
          }
      }
    case any =>
      log.error(s"OrderByPageLikes received unknown message: $any.")
  }

}
