package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection

/**
  * Factory for [[me.reminisce.gameboard.questions.OrderByPageLikes]]
  */
object OrderByPageLikes {
  /**
    * Creates an OrderByPageLikes question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new OrderByPageLikes(database))
}

/**
  * OrderByPageLikes question generator
  * @param db database from which to take the data
  */
class OrderByPageLikes(db: DefaultDB) extends OrderQuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
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
