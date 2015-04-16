package me.reminisce.service.questiongen

import akka.actor.Props
import me.reminisce.entities.Entities
import me.reminisce.entities.Entities.SpecificQuestionType._
import me.reminisce.entities.Entities.{MultipleChoiceQuestion, Possibility, Question}
import me.reminisce.mongodb.MongoDBEntities.{FBComment, FBPost}
import me.reminisce.service.questiongen.QuestionGenerator.{FinishedQuestionCreation, FailedToCreateQuestion, CreateQuestion}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONDocument

import scala.concurrent.{Future, Promise}
import scala.util.{Failure, Random, Success}

/**
 * Created by roger on 20/11/14.
 */

object WhoMadeThisCommentOnYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoMadeThisCommentOnYourPost(database))
}

class WhoMadeThisCommentOnYourPost(db: DefaultDB) extends PostQuestionGenerator(db) {

  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      retrievePostWithEnoughComments(user_id).onComplete {
        case Success(post) =>
          val commentSelection = Random.shuffle(post.comments.get).slice(0, 4)
          val possibilities = commentSelection.map {
            c =>
              Possibility(Some(c.from.user_name), Some(""), Some(c.from.user_id))
          }
          val answer = possibilities.head

          val answerComment = commentSelection.filter(c => c.from.user_id == answer.fb_id.get).head

          val randomPossibilities = Random.shuffle(possibilities).toVector

          val question = Question("WhoMadeThisCommentOnThePost", Some(List(post.message.get, answerComment.message)))

          val mc = MultipleChoiceQuestion(answerComment.id,
            user_id, question, randomPossibilities,
            randomPossibilities.indexOf(answer))
          client ! FinishedQuestionCreation(mc)
        case Failure(t) =>
          client ! FailedToCreateQuestion("Could not create question for WhoMadeThisCommentOnYourPost " + t.getMessage, MCWhoMadeThisCommentOnYourPost)
      }

  }

  def retrievePostWithEnoughComments(user_id: String): Future[FBPost] = {
    val promise = Promise[FBPost]()
    val limit = QuestionParameters.NumberOfTrials
    def recurse(counter: Int): Unit = {
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true"),
        "comments" -> BSONDocument("$exists" -> "true"),
        "comments_count" -> BSONDocument("$gt" -> 3)
      )
      val document = getDocument(db, collection, query)
      document.onComplete {
        case Success(Some(post:FBPost)) =>
          val distinct = distinctComments(post.comments.get, Set())
          if (distinct.length >= 4) {
            promise.success(post.copy(comments = Some(distinct)))
          } else if (counter > limit) {
            promise.failure(new Exception("there are not enough comments"))
          } else {
            recurse(counter + 1)
          }
        case Success(None) =>
          log.error("There where no comments on that post")
          promise.failure(new Exception("Something went wrong"))
        case Failure(t) =>
          log.error("could not open document for WhoMadeThisCommentOnYourPost ")
          promise.failure(t)

      }
    }
    def distinctComments(comments: List[FBComment], userSet: Set[String]): List[FBComment] = {
      comments match {
        case x :: xs =>
          if (!userSet.contains(x.from.user_id)) {
            x :: distinctComments(xs, userSet + x.from.user_id)
          } else {
            distinctComments(xs, userSet)
          }
        case Nil => Nil
      }
    }
    recurse(0)
    promise.future
  }
}

