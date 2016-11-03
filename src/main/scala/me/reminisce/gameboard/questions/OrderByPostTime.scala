package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoCollections
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection

/**
  * Factory for [[me.reminisce.gameboard.questions.OrderByPostTime]]
  */
object OrderByPostTime {

  /**
    * Creates an OrderByPostTime question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new OrderByPostTime(database))
}

/**
  * OrderByPostTime question generator
  *
  * @param db database from which to take the data
  */
class OrderByPostTime(db: DefaultDB) extends OrderQuestionGenerator {

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
            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
            val ordered = postsList.take(itemsToOrder).sortBy {
              post =>
                val date = formatter.parseDateTime(post.createdTime.get)
                date.getMillis
            }.map(p => subjectFromPost(p))
            OrderQuestionGenerator.generateSubjectsWithId(ordered) match {
              case (subjectsWithId, answer) =>
                val gameQuestion = OrderQuestion(userId, Order, ORDPostTime, None, subjectsWithId, answer)
                client ! FinishedQuestionCreation(gameQuestion)
            }
          }
      }
    case any =>
      log.error(s"Unknown message : $any.")
  }

}
