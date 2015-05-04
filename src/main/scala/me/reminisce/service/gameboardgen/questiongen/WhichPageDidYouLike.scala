package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{ActorRef, Props}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPage, FBPageLike}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Random, Success}


object WhichPageDidYouLike {
  def props(database: DefaultDB): Props =
    Props(new WhichPageDidYouLike(database))

}


class WhichPageDidYouLike(db: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(user_id, item_id) =>
      val client = sender()
      val pagesCollection = db[BSONCollection](MongoDatabaseService.fbPagesCollection)
      val likesCollection = db[BSONCollection](MongoDatabaseService.fbPageLikesCollection)
      val pageQuery = BSONDocument(
        "page_id" -> item_id
      )
      fetchPage(pagesCollection, item_id, client) {
        case Some(page) =>
          fetchLikedPages(likesCollection, user_id, client) {
            list =>
              val ids = list.map(fbPageLike => fbPageLike.page_id)
              val queryNotLiked = BSONDocument(
                "page_id" -> BSONDocument("$nin" -> ids)
              )
              getDocuments[FBPage](db, pagesCollection, queryNotLiked, 3).onComplete {
                case Success(listPages) =>
                  if (listPages.length < 3) {
                    client ! NotEnoughData(s"Unable to create question : not enough not liked pages.")
                  } else {
                    val possibilities = (page :: listPages).map {
                      pge =>
                        val url = pge.photos match {
                          case Some(p) => p.source
                          case None => None
                        }
                        Possibility(pge.name.get, url, Some(pge.page_id))
                    }
                    val answer = possibilities.head
                    val shuffled = Random.shuffle(possibilities)
                    val subject = PageSubject(answer.name, "http://facebook.com/" + answer.fbId.get, answer.imageUrl)
                    val gameQuestion = MultipleChoiceQuestion(user_id, MultipleChoice, MCWhichPageDidYouLike, subject, shuffled, shuffled.indexOf(answer))
                    client ! FinishedQuestionCreation(gameQuestion)
                  }
                case Failure(e) =>
                  client ! MongoDBError(s"${e.getMessage}")
              }
          }
        case None =>
          client ! NotEnoughData("Page not found.")
      }
    case x => log.error(s"WhichPageDidYouLike received a unexpected message $x")
  }

  def fetchLikedPages(pageLikesCollection: BSONCollection, user_id: String, client: ActorRef)(f: List[FBPageLike] => Unit): Unit = {
    val query = BSONDocument("user_id" -> user_id)
    pageLikesCollection.find(query).cursor[FBPageLike].collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
    }
  }

  def fetchPage(pagesCollection: BSONCollection, page_id: String, client: ActorRef)(f: Option[FBPage] => Unit): Unit = {
    val query = BSONDocument("page_id" -> page_id)
    pagesCollection.find(query).one[FBPage].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
    }
  }

}
