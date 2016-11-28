package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.gameboard.board.GameboardEntities.{TLWhenDidYouShareThisPost, Timeline, TimelineQuestion}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import me.reminisce.gameboard.questions.TimeQuestionGenerator._
import me.reminisce.gameboard.questions._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Random}

/**
  * Factory for [[me.reminisce.gameboard.questions.WhenDidYouShareThisPostWithDifficulty]]
  */
object WhenDidYouShareThisPostWithDifficulty {

  /**
    * Creates a WhenDidYouShareThisPostWithDifficulty question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhenDidYouShareThisPostWithDifficulty(database))
}

/**
  * WhenDidYouShareThisPostWithDifficulty question generator
  *
  * @param db database from which to take the data
  */
class WhenDidYouShareThisPostWithDifficulty(db: DefaultDB) extends TimeQuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    *
    * @return Nothing
    */
  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val query = BSONDocument( 
        "userId" -> userId 
  //      "postId" -> itemId
      )
      
      val postCollection = db[BSONCollection](MongoCollections.fbPosts)
      getDocuments[FBPost](db, postCollection, query, 20).onComplete {
        case Success(postsList) =>
          val maybePost = selectPost(getDifficultyForQuestion(userId), postsList)
          maybePost match {
            case Some(post) =>
              val dateString = post.createdTime.getOrElse("1970-01-01'T'00:00:00+0000")
              val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
              val actualDate = formatter.parseDateTime(dateString)
              val threshold = 0
              val postSubject = QuestionGenerator.subjectFromPost(post)
              generateRange(actualDate) match {
                case (min, default, max, unit, step) =>
                  val tlQuestion = TimelineQuestion(userId, Timeline, TLWhenDidYouShareThisPost, Some(postSubject),
                    actualDate.toString(formatter), min.toString(formatter), max.toString(formatter),
                    default.toString(formatter), unit, step, threshold)
                  client ! FinishedQuestionCreation(tlQuestion)
              }
            case None =>
              client ! NotEnoughData(s"Post not found : $maybePost")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
        case any =>
          client ! MongoDBError(s"Unknown error: $any.")
      }
  }
  
  /**
   * Select a post in the {@postList} according to the difficulty
   * 
   * @param difficulty The difficulty of the question
   * @param postsList List of posts from which the answer will be taken
   * 
   * @return A post
   */
  private def selectPost(difficulty: Double, postsList: List[FBPost]): Option[FBPost] = {
    postsList.filter { _.createdTime.nonEmpty} match {
      case Nil => None
      case x::Nil => Some(x)
      case xs => {
        val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)     
        val sorted = xs.map { x => {
            (x, formatter.parseDateTime(x.createdTime.get))
          }
        }.sortBy(_._2)
        Option(Random.shuffle(sorted.take(Math.max(1,(-15*difficulty + 20).toInt))).head._1)
      }
    }
  }

}