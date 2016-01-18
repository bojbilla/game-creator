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


object OrderByPostLikesNumber {

  def props(database: DefaultDB): Props =
    Props(new OrderByPostLikesNumber(database))
}

class OrderByPostLikesNumber(db: DefaultDB) extends OrderQuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      fetchPosts(postsCollection, userId, itemIds, client) {
        postsList =>
          if (postsList.length < itemsToOrder) {
            client ! NotEnoughData(s"Not enough posts in list.")
          } else {
            val ordered = postsList.take(itemsToOrder).sortBy(_.likesCount).map(subjectFromPost)
            OrderQuestionGenerator.generateSubjectsWithId(ordered) match {
              case (subjectsWithId, answer) =>
                val gameQuestion = OrderQuestion(userId, Order, ORDPostLikesNumber, None, subjectsWithId, answer)
                client ! FinishedQuestionCreation(gameQuestion)
            }
          }
      }
    case any =>
      log.error(s"OrderByPostLikesNumber received an unsupported message: $any.")
  }

}
