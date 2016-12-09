package me.reminisce.gameboard.questions

import akka.actor.Props
import com.github.nscala_time.time.Imports._
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.gameboard.board.GameboardEntities.{TLWhenDidYouLikeThisPage, Timeline, TimelineQuestion}
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import me.reminisce.gameboard.questions.TimeQuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success, Random}


/**
  * Factory for [[me.reminisce.gameboard.questions.WhenDidYouLikeThisPageWithDifficulty]]
  */
object WhenDidYouLikeThisPageWithDifficulty {

  /**
    * Creates a WhenDidYouLikeThisPageWithDifficulty question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhenDidYouLikeThisPageWithDifficulty(database))
}

/**
  * WhenDidYouLikeThisPageWithDifficulty question generator
  *
  * @param db database from which to take the data
  */
class WhenDidYouLikeThisPageWithDifficulty(db: DefaultDB) extends TimeQuestionGenerator {

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
      )
      val pageLikesCollection = db[BSONCollection](MongoCollections.fbPageLikes)      
      
      for {
        pageLikeList <- getDocuments[FBPageLike](db, pageLikesCollection, query, 20)//pageLikesCollection.find(query).one[FBPageLike]
        maybePageLike = selectPage(None, pageLikeList)
      } yield {
        maybePageLike match {
          case Some(pageLike) => {
            val actualDate = pageLike.likeTime
            generateRange(actualDate) match {
              case (min, default, max, unit, step) =>
                val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
                val pageSelector = BSONDocument("pageId" -> pageLike.pageId)
                val maybePage = pagesCollection.find(pageSelector).one[FBPage]
                maybePage.onComplete{
                  case Success(page) =>{
                    val threshold = 0
                    val pageSubject = QuestionGenerator.subjectFromPage(page.head)
                    val formatter = DateTimeFormat.forPattern("yyyy-MM-dd'T'HH:mm:ssZ").withZone(DateTimeZone.UTC)
                    client ! FinishedQuestionCreation(TimelineQuestion(userId, Timeline, TLWhenDidYouLikeThisPage, Some(pageSubject),
                      actualDate.toString(formatter), min.toString(formatter), max.toString(formatter),
                      default.toString(formatter), unit, step, threshold))
                  }
                  case Failure(e) => client ! MongoDBError(s"${e.getMessage}")
                }
              }
          }
          case None => {
            client ! NotEnoughData(s"Pagelike not retrieved in `selectPage`")
          }
        }
      }
  }
  
  /**
   * Select a pageLike in the {@pageLikeList} according to the difficulty
   * 
   * @param difficulty The difficulty of the question
   * @param pageLikeList List of pagesLike from which the result will be taken
   * 
   * @return A pageLike
   */
  private def selectPage(difficulty: Option[Double], pageLikeList: List[FBPageLike]): Option[FBPageLike] = {
    val sorted = pageLikeList.sortBy { _.likeTime }
    Option(Random.shuffle(sorted.take(Math.max(1,(-15*difficulty.getOrElse(0.0) + 20).toInt))).head)
  }
  
}
