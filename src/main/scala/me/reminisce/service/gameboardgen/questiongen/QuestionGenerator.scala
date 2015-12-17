package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{Actor, ActorLogging, ActorRef}
import me.reminisce.mongodb.MongoDBEntities.{FBAttachment, FBPage, FBPageLike, FBPost}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.MongoDBError
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

object QuestionGenerator {

  case class CreateQuestion(userId: String, itemId: String) extends RestMessage

  case class CreateQuestionWithMultipleItems(userId: String, itemIds: List[String]) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class MongoDBError(message: String)

  case class NotEnoughData(message: String)

  def subjectFromPost(post: FBPost): PostSubject = {
    post.tpe match {
      case Some(tpe) =>
        tpe match {
          case "photo" =>
            val text = textFromPost(post)
            val imageUrl = srcFromAttachments(post.attachments)
            val facebookImageUrl = post.link
            ImagePostSubject(text, imageUrl, facebookImageUrl, from = post.from)
          case "video" =>
            val text = textFromPost(post)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            VideoPostSubject(text, thumbnailUrl, url, from = post.from)
          case "link" =>
            val text = textFromPost(post)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            LinkPostSubject(text, thumbnailUrl, url, from = post.from)
          case _ =>
            val text = textFromPost(post)
            TextPostSubject(text, from = post.from)
        }
      case None =>
        val text = textFromPost(post)
        TextPostSubject(text, from = post.from)
    }
  }

  def textFromPost(post: FBPost): String = {
    post.message match {
      case Some(message) =>
        message + {
          post.story match {
            case Some(story) => "\n" + story
            case None => ""
          }
        }
      case None => post.story.getOrElse("")
    }
  }

  def srcFromAttachments(attachmentOpt: Option[List[FBAttachment]]): Option[String] = {
    for {
      attachmentList <- attachmentOpt
      attachment <- attachmentList.headOption
      media <- attachment.media
    } yield media.src
  }

  def subjectFromPage(page: FBPage): PageSubject = {
    val url = page.photos match {
      case Some(p) => p.source
      case None => None
    }
    val name = page.name.getOrElse("")
    PageSubject(name, page.pageId, url)
  }
}

abstract class QuestionGenerator extends Actor with ActorLogging {
  protected def getDocuments[T](db: DefaultDB,
                                collection: BSONCollection,
                                query: BSONDocument, quantity: Int)
                               (implicit reader: BSONDocumentReader[T]): Future[List[T]] = {
    val futureCount = db.command(Count(collection.name, Some(query)))
    futureCount.flatMap { count =>
      val skip = if (count - quantity > 0) Random.nextInt(count - quantity) else 0
      collection.find(query).
        options(QueryOpts(skipN = skip)).cursor[T].collect[List](quantity)

    }
  }

  protected def fetchPosts(postsCollection: BSONCollection, userId: String, postIds: List[String], client: ActorRef)(f: List[FBPost] => Unit): Unit = {
    val query = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> postIds))
    postsCollection.find(query).cursor[FBPost].collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Uknown error : $any.")
    }
  }

  protected def fetchPages(pagesCollection: BSONCollection, pageIds: List[String]): Future[List[FBPage]] = {
    val query = BSONDocument("pageId" -> BSONDocument("$in" -> pageIds))
    pagesCollection.find(query).cursor[FBPage].collect[List]()
  }

  def fetchPages(pagesCollection: BSONCollection, pageIds: List[String], client: ActorRef)(f: List[FBPage] => Unit): Unit = {
    fetchPages(pagesCollection, pageIds).onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Uknown error : $any.")
    }
  }

  protected def fetchLikedPages(pageLikesCollection: BSONCollection, userId: String,
                                pageIds: Option[List[String]]): Future[List[FBPageLike]] = {
    val query = pageIds match {
      case Some(list) =>
        BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$in" -> list))
      case None =>
        BSONDocument("userId" -> userId)
    }
    pageLikesCollection.find(query).cursor[FBPageLike].collect[List]()
  }

  protected def fetchLikedPages(pageLikesCollection: BSONCollection, userId: String, client: ActorRef, pageIds: Option[List[String]] = None)
                               (f: List[FBPageLike] => Unit): Unit = {
    fetchLikedPages(pageLikesCollection, userId: String, pageIds).onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Uknown error : $any.")
    }
  }

  protected def fetchPage(pagesCollection: BSONCollection, userId: String, client: ActorRef)(f: Option[FBPage] => Unit): Unit = {
    val query = BSONDocument("pageId" -> userId)
    pagesCollection.find(query).one[FBPage].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Uknown error : $any.")
    }
  }
}
