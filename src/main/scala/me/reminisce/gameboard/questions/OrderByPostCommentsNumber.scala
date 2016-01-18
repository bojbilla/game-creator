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


object OrderByPostCommentsNumber {

  def props(database: DefaultDB): Props =
    Props(new OrderByPostCommentsNumber(database))
}

class OrderByPostCommentsNumber(db: DefaultDB) extends OrderQuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      fetchPosts(postsCollection, userId, itemIds, client) {
        postsList =>
          if (postsList.length < itemsToOrder) {
            client ! NotEnoughData(s"Not enough posts in list.")
          } else {
            val ordered = postsList.take(itemsToOrder).sortBy(_.commentsCount).map(subjectFromPost)
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
