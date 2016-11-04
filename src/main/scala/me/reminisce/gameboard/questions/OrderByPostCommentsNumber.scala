package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoCollections
import me.reminisce.gameboard.board.GameboardEntities.{ORDPostCommentsNumber, Order, OrderQuestion}
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection

/**
  * Factory for [[me.reminisce.gameboard.questions.OrderByPostCommentsNumber]]
  */
object OrderByPostCommentsNumber {

  /**
    * Creates an OrderByPostCommentsNumber question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new OrderByPostCommentsNumber(database))
}

/**
  * OrderByPostCommentsNumber question generator
  *
  * @param db database from which to take the data
  */
class OrderByPostCommentsNumber(db: DefaultDB) extends OrderQuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    *
    * @return Nothing
    */
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoCollections.fbPosts)
      fetchPosts(postsCollection, userId, itemIds, client) {
        postsList =>
          if (postsList.length < itemsToOrder) {
            client ! NotEnoughData(s"Not enough posts in list.")
          } else {
            val ordered = postsList.take(itemsToOrder).sortBy(_.commentsCount).map(p => subjectFromPost(p))
            OrderQuestionGenerator.generateSubjectsWithId(ordered) match {
              case (subjectsWithId, answer) =>
                val gameQuestion = OrderQuestion(userId, Order, ORDPostCommentsNumber, None, subjectsWithId, answer)
                client ! FinishedQuestionCreation(gameQuestion)
            }
          }
      }
    case any =>
      log.error(s"OrderByPostCommentsNumber received an unknown message : $any.")
  }

}
