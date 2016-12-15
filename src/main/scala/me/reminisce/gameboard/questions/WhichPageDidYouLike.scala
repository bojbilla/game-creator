package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.FBPage
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

/**
  * Factory for [[me.reminisce.gameboard.questions.WhichPageDidYouLike]]
  */
object WhichPageDidYouLike {

  /**
    * Creates a WhichPageDidYouLike question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhichPageDidYouLike(database))
}

/**
  * WhichPageDidYouLike question generator
  *
  * @param db database from which to take the data
  */
class WhichPageDidYouLike(db: DefaultDB) extends QuestionGenerator {

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
      val pagesCollection = db[BSONCollection](MongoCollections.fbPages)
      val likesCollection = db[BSONCollection](MongoCollections.fbPageLikes)
      fetchPage(pagesCollection, itemId, client) {
        case Some(page) =>
          fetchLikedPages(likesCollection, userId, client) {
            list =>
              val ids = list.map(fbPageLike => fbPageLike.pageId)
              val queryNotLiked = BSONDocument(
                "pageId" -> BSONDocument("$nin" -> ids)
              )
              getDocuments[FBPage](db, pagesCollection, queryNotLiked, 40).onComplete {
                case Success(listPages) =>
                  if (listPages.length < 3) {
                    client ! NotEnoughData(s"Unable to create question : not enough not liked pages.")
                  } else {
                    val choices = getChoices(None, listPages)
                    val possibilities = (page :: choices).map {
                      pge =>
                        val url = pge.photos match {
                          case Some(p) => p.source
                          case None => None
                        }
                        Possibility(pge.name.get, url, "Page", Some(pge.pageId))
                    }
                    possibilities.headOption match {
                      case Some(answer) =>
                        val shuffled = Random.shuffle(possibilities)
                        val gameQuestion = MultipleChoiceQuestion(userId, MultipleChoice, MCWhichPageDidYouLike, None, shuffled, shuffled.indexOf(answer))
                        client ! FinishedQuestionCreation(gameQuestion)
                      case None =>
                        client ! NotEnoughData(s"This should not happen, but the possibilities were empty.")
                    }
                  }
                case Failure(e) =>
                  client ! MongoDBError(s"${e.getMessage}")
                case any =>
                  client ! MongoDBError(s"Unknown error : $any.")
              }
          }
        case None =>
          client ! NotEnoughData(s"Page not found. $itemId")
      }
    case any =>
      log.error(s"WhichPageDidYouLike received a unexpected message $any")
  }
  
  /**
   * Get 3 choices. The size of the pool from which there are chosen follows:
   * y = -30*difficulty + 40 (This is arbitrary)
   * 
   * @param difficulty User difficulty for question
   * @param listPages List of potential FBpage as choices
   * @return 3 FBpages as a List
   */
  private def getChoices(difficulty: Option[Double], listPages: List[FBPage]): List[FBPage] = {
    val sortedList = listPages.sortBy(-_.likesNumber)
    val pool = Random.shuffle(sortedList.take((-30*difficulty.getOrElse(0.0)+40).toInt))
    pool.take(3)
  }
}
