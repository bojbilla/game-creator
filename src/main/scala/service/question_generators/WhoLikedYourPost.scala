package service.question_generators

import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import database.MongoDatabaseService
import entities.Entities.SpecificQuestionType._
import entities.Entities.{MultipleChoiceQuestion, Possibility, Question}
import mongodb.MongoDBEntities.{FBLike, FBPost}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count
import server.domain.RestMessage
import service.question_generators.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}

import scala.concurrent.{ExecutionContextExecutor, Future, Promise}
import scala.util.{Failure, Random, Success}

/**
 * Created by roger on 19/11/14.
 */

object WhoLikedYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoLikedYourPost(database))

  case class CreatedWhoLikedYourPost(mc: MultipleChoiceQuestion) extends RestMessage

}


class WhoLikedYourPost(database: DefaultDB) extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  val collection = database[BSONCollection](MongoDatabaseService.fbPostsCollection)

  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "message" -> BSONDocument("$exists" -> "true")
      )
      getDocument(database, collection, query).onComplete {
        case Success(s) => s match {
          case Some(post) =>
            post.likes match {
              case Some(Nil) =>
                client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id", MCWhoLikedYourPost)
              case Some(likers) => getLikesFromOtherPosts(post, likers.toSet).onComplete {
                case Success(others) =>
                  val question = Question("WhoLikedThisPost", Some(List(post.message.getOrElse(""))))
                  val possibilities = Possibility(Some(Random.shuffle(likers).head.user_name), Some(""), Some(Random.shuffle(likers).head.user_id)) +: others.slice(0, 3).map { other =>
                    Possibility(Some(other.user_name), Some(""), Some(other.user_id))
                  }.toVector
                  val answerPossibility = possibilities(0)
                  val randomPossibilities = Random.shuffle(possibilities)
                  val mc = MultipleChoiceQuestion(post.post_id,
                    user_id, question, randomPossibilities,
                    randomPossibilities.indexOf(answerPossibility))
                  client ! FinishedQuestionCreation(mc)
                case Failure(e) =>
                  client ! FailedToCreateQuestion(e.getMessage + " -> No people who liked post found", MCWhoLikedYourPost)
              }
              case None =>
                client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id", MCWhoLikedYourPost)
            }

          case None =>
            client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id", MCWhoLikedYourPost)
        }
        case Failure(e) =>
          log.error(s"Failed to create CreatedWhoLikedYourPost due to ${e.getMessage}")
          client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id", MCWhoLikedYourPost)
      }


    case _ => log.error(s"WhoLikedYourPost received a unexpected message ")
  }

  def getLikesFromOtherPosts(post: FBPost, likers: Set[FBLike]): Future[Set[FBLike]] = {
    val promise = Promise[Set[FBLike]]()
    val limit = QuestionParameters.NumberOfTrials
    val query = BSONDocument(
      "user_id" -> post.user_id,
      "like_count" -> BSONDocument("$gt" -> 5),
      "message" -> BSONDocument("$exists" -> "true"),
      "post_id" -> BSONDocument("$ne" -> post.post_id)
    )
    def recurs(likers: Set[FBLike], nonLikers: Set[FBLike], counter: Int = 0): Unit = {
      if (nonLikers.size >= 3) {
        promise.success(nonLikers)
      } else if (counter > limit) {
        promise.failure(new Exception("Unable to find non likers"))
      } else {
        getDocument(database, collection, query).onComplete {
          case Success(Some(otherPost)) =>
            otherPost.likes match {
              case Some(likes) =>
                val newNonLikers = likes.toSet &~ likers
                recurs(likers, nonLikers ++ newNonLikers, counter + 1)
              case None =>
                recurs(likers, nonLikers, counter + 1)
            }
          case Success(None) =>
            promise.failure(new Exception("No posts found"))

          case Failure(t) =>
            promise.failure(t)
        }
      }
    }
    recurs(likers, Set())
    promise.future
  }

  def getDocument(db: DefaultDB,
                  collection: BSONCollection,
                  query: BSONDocument): Future[Option[FBPost]] = {
    val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      val skip = Random.nextInt(count)
      collection.find(query).
        options(QueryOpts(skipN = skip)).one[FBPost]

    }
  }
}
