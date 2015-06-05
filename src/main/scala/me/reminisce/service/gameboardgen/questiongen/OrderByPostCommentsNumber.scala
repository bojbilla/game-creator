package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, SubjectWithId}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection

import scala.util.Random


object OrderByPostCommentsNumber {

  def props(database: DefaultDB): Props =
    Props(new OrderByPostCommentsNumber(database))
}

class OrderByPostCommentsNumber(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      fetchPosts(postsCollection, userId, itemIds, client) {
        postsList =>
          if (postsList.length < 4) {
            client ! NotEnoughData(s"Not enough posts in list.")
          } else {
            val ordered = postsList.take(4).sortBy(_.commentsCount)
            val answer = (0 until 4).toList
            val subjectsWithId = Random.shuffle(ordered.zip(answer).map {
              case (post, id) => SubjectWithId(subjectFromPost(post), id)
            })
            val gameQuestion = OrderQuestion(userId, Order, ORDPostCommentsNumber, None, subjectsWithId, answer)
            client ! FinishedQuestionCreation(gameQuestion)
          }
      }
  }

}
