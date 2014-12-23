package service.question_generators

import akka.actor.{Actor, ActorContext, ActorLogging, Props}
import database.MongoDatabaseService
import entities.Entities.{Possibility, Question, MultipleChoiceQuestion}
import mongodb.MongoDBEntities.{FBLike, FBFrom, FBPost}
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.BSONDocument
import reactivemongo.core.commands.Count
import server.domain.Domain.Done
import server.domain.RestMessage
import service.GameGenerator.{CreateQuestion, CreatedWhoLikedYourPost, FailedToCreateQuestion}

import scala.concurrent.{Promise, ExecutionContextExecutor, Future}
import scala.util.{Failure, Success, Random}

/**
 * Created by roger on 19/11/14.
 */

object WhoLikedYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoLikedYourPost(database))

  case class CreatedWhoLikedYourPost(mc: MultipleChoiceQuestion) extends RestMessage
}



class WhoLikedYourPost(database: DefaultDB) extends Actor with ActorLogging{
  implicit def dispatcher: ExecutionContextExecutor =  context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  val collection = database[BSONCollection](MongoDatabaseService.fbPostsCollection)

  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
//        "like_count" -> BSONDocument( "$gt" -> 5),
        "message" -> BSONDocument("$exists" -> "true")
      )
      log.error(s"GameGenerator we tried " + user_id)
      val doc = getDocument(database, collection, query)
      doc.onComplete {
        case Success(doc0) =>
          log.error("wat")
        case Failure(e) => log.error("we failed: " + e.getMessage)

      }
      getDocument(database, collection, query).onComplete {
        case Success(s) => s match {
          case Some(post) =>
            log.error(s"GameGenerator we got")

            post.likes match {
              case Some(likers) => getLikesFromOtherPosts(post, likers.toSet).onComplete {
                case Success(others) =>
                  val question = Question("Which of your friends liked this post?", post.message
                    , None)

                  val possibilities = Possibility(Some(Random.shuffle(likers).head.user_name), None) +: others.slice(0, 3).map { other =>
                    Possibility(Some(other.user_name), None)
                  }.toVector
                  val answerPossibility = possibilities(0)
                  val randomPossibilities = Random.shuffle(possibilities)
                  val mc = MultipleChoiceQuestion("somerandomstring",
                    user_id, question, randomPossibilities,
                    randomPossibilities.indexOf(answerPossibility))
                  log.info(s"post id: ${post.post_id}")
                  client ! CreatedWhoLikedYourPost(mc)

                case Failure(e) =>
                  log.error(s"GameGenerator we got nothing")

                  client ! FailedToCreateQuestion(e.getMessage)
              }
              case None =>
                log.error(s"GameGenerator we got nothing")

                client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id")
            }

          case None =>
            log.error(s"GameGenerator we got nothing")

            client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id")
        }
        case Failure(e) =>
          log.error(s"Failed to create CreatedWhoLikedYourPost due to ${e.getMessage}")
          client ! FailedToCreateQuestion(s"Not enough posts with likes > 5 for user: $user_id")
      }


    case _ => log.error(s"GameGenerator received a unexpected message")
  }

  def getLikesFromOtherPosts(post: FBPost, likers: Set[FBLike]): Future[Set[FBLike]] = {
    val promise = Promise[Set[FBLike]]()
    val limit = 1000
    val query = BSONDocument(
      "user_id" -> post.user_id,
      "like_count" -> BSONDocument( "$gt" -> 5),
      "message" -> BSONDocument("$exists" -> "true"),
      "post_id" -> BSONDocument("$ne" -> post.post_id)
    )
    def recurs(likers: Set[FBLike], nonLikers: Set[FBLike], counter: Int = 0): Unit ={
      log.error(s"GameGenerator we recurse")

      if (nonLikers.size >= 3){
        promise.success(nonLikers)
      } else if(counter > limit) {
        promise.failure(new Exception("Unable to find non likers"))
      } else {
        val others = getDocument(database, collection, query).map(
          postO => postO.flatMap(post => post.likes.map(
            likes =>
              likes.toSet &~ likers
          ))
        )
        others.map{
          case Some(newNonLikers) => recurs(likers, nonLikers ++ newNonLikers, counter + 1)
          case None => recurs(likers, nonLikers, counter + 1)
        }
      }
    }
    recurs(likers, Set())

    promise.future
  }

  def getDocument(db: DefaultDB,
                  collection: BSONCollection,
                  query: BSONDocument): Future[Option[FBPost]] = {
    log.error(s"GameGenerator we get document")

    val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      log.info("we count " + count)
      val skip = Random.nextInt(count)
      collection.find(query).
        options(QueryOpts(skipN = skip)).one[FBPost]

    }
  }
}
