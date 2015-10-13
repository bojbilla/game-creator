package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.service.gameboardgen.GameboardEntities.OrderQuestion
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
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
            val (subjectsWithId, answer) = OrderQuestionGenerator.generateSubjectsWithId(ordered)
            val gameQuestion = OrderQuestion(userId, Order, ORDPostCommentsNumber, None, subjectsWithId, answer)
            client ! FinishedQuestionCreation(gameQuestion)
          }
      }
  }

}
