package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.OrderQuestion
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection


object OrderByPostTime {

  def props(database: DefaultDB): Props =
    Props(new OrderByPostTime(database))
}

class OrderByPostTime(db: DefaultDB) extends OrderQuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
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
            }.map(subjectFromPost)
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
