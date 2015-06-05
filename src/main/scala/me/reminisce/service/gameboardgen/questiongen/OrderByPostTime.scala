package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoDatabaseService
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{OrderQuestion, SubjectWithId}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection

import scala.util.Random


object OrderByPostTime {

  def props(database: DefaultDB): Props =
    Props(new OrderByPostTime(database))
}

class OrderByPostTime(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestionWithMultipleItems(userId, itemIds) =>
      val client = sender()
      val postsCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      fetchPosts(postsCollection, userId, itemIds, client) {
        postsList =>
          if (postsList.length < 4) {
            client ! NotEnoughData(s"Not enough posts in list.")
          } else {
            val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
            val ordered = postsList.take(4).sortBy {
              post =>
                val date = formatter.parseDateTime(post.createdTime.get)
                date.getMillis
            }
            val answer = (0 until 4).toList
            val subjectsWithId = Random.shuffle(ordered.zip(answer).map {
              case (post, id) => SubjectWithId(subjectFromPost(post), id)
            })
            val gameQuestion = OrderQuestion(userId, Order, ORDPostTime, None, subjectsWithId, answer)
            client ! FinishedQuestionCreation(gameQuestion)
          }
      }
  }

}
