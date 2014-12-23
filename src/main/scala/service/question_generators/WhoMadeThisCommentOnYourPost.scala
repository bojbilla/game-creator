package service.question_generators

import akka.actor.{ActorLogging, Actor, Props}
import database.MongoDatabaseService
import entities.Entities.{Possibility, Question, MultipleChoiceQuestion}
import mongodb.MongoDBEntities.{FBComment, FBPost}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument
import server.domain.RestMessage
import service.GameGenerator.{FailedToCreateQuestion, CreatedWhoLikedYourPost, CreateQuestion}
import service.question_generators.WhoMadeThisCommentOnYourPost.CreatedWhoMadeThisCommentOnYourPost

import scala.concurrent.{Promise, Future}
import scala.util.{Failure, Random, Success}

/**
 * Created by roger on 20/11/14.
 */

object WhoMadeThisCommentOnYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoMadeThisCommentOnYourPost(database))
  case class CreatedWhoMadeThisCommentOnYourPost(mc: MultipleChoiceQuestion) extends RestMessage
}
class WhoMadeThisCommentOnYourPost(db: DefaultDB) extends PostQuestionGenerator(db) {

  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true"),
        "comments_count" -> BSONDocument( "$gt" -> 3)
      )


      val mcQuestion = retrievePostWithEnoughComments(user_id).map {

        post => post.message.flatMap {
          message => post.comments.flatMap {
            comments =>

              val commentSelection = Random.shuffle(comments).slice(0, 4)

              val possibilities = commentSelection.map {
                c =>
                  Possibility(Some(c.from.user_id), Some(c.from.user_name))
              }
              val answer = possibilities.head

              val answerComment = commentSelection.filter(c => c.from.user_id == answer.text.get).head

              val randomPossibilities = Random.shuffle(possibilities).toVector
              val question = Question(
                s"Who made the comment?\n${answerComment.message}",
                Some(s"On your post: $message")
                , None)
              Some(MultipleChoiceQuestion("somerandomstring",
                user_id, question, randomPossibilities,
                randomPossibilities.indexOf(answer)))


          }
        }
      }
      mcQuestion.map{
        case Some(q) => client !  CreatedWhoMadeThisCommentOnYourPost(q)
        case None =>
          log.error("failed to create question")
          client ! FailedToCreateQuestion(s"Not enough posts with enough comments for user: $user_id")
      }
    case _ =>
  }

  def retrievePostWithEnoughComments(user_id: String): Future[FBPost] = {
    val promise = Promise[FBPost]()
    val limit = 1000
    def recurse(counter: Int): Unit = {
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true"),
        "comments_count" -> BSONDocument( "$gt" -> 3)
      )

      getDocument(db, collection, query).map{ postO => postO.map {
        post =>
        post.comments.map {
          comments =>
            val distinct = distinctComments(comments, Set())
            if (distinct.length >= 4){
              promise.success(post.copy(comments = Some(distinct)))
            } else if (counter > limit){
              promise.failure(new Exception("there are not enough comments"))
            } else {
              recurse(counter + 1)
            }
        }
      }
      }
      def distinctComments(comments: List[FBComment], userSet: Set[String]): List[FBComment] = {
        comments match {
          case x :: xs =>
            if(!userSet.contains(x.from.user_id)){
            x :: distinctComments(xs, userSet + x.from.user_id)
          } else {
              distinctComments(xs, userSet)
            }
          case Nil => Nil
        }
      }
    }
    recurse(0)
    promise.future
  }
}

