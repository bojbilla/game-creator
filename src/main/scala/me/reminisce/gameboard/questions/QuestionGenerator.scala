package me.reminisce.gameboard.questions

import akka.actor.{Actor, ActorLogging, ActorRef}
import akka.event.Logging
import me.reminisce.database.MongoDBEntities.{FBAttachment, FBPage, FBPageLike, FBPost}
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator.MongoDBError
import me.reminisce.server.domain.RestMessage
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.{Failure, Random, Success}

/**
  * Question generation utility methods and case classes for message passing.
  */
object QuestionGenerator {

  case class CreateQuestion(userId: String, itemId: String) extends RestMessage

  case class CreateQuestionWithMultipleItems(userId: String, itemIds: List[String]) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class MongoDBError(message: String)

  case class NotEnoughData(message: String)

  /**
    * Extract the right subject type from a post
    * @param post post from which the subject must be extracted
    * @return a subject representing the page
    */
  def subjectFromPost(post: FBPost, includeStory: Boolean = true): PostSubject = {
    post.tpe match {
      case Some(tpe) =>
        tpe match {
          case "photo" =>
            val text = textFromPost(post, includeStory)
            val attachUrl = srcFromAttachments(post.attachments)
            val facebookImageUrl = post.link
            val imageUrl = attachUrl match {
              case Some(url) =>
                Some(url)
              case None =>
                post.picture
            }
            ImagePostSubject(text, imageUrl, facebookImageUrl, from = post.from)
          case "video" =>
            val text = textFromPost(post, includeStory)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            VideoPostSubject(text, thumbnailUrl, url, from = post.from)
          case "link" =>
            val text = textFromPost(post, includeStory)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            LinkPostSubject(text, thumbnailUrl, url, from = post.from)
          case _ =>
            val text = textFromPost(post, includeStory)
            TextPostSubject(text, from = post.from)
        }
      case None =>
        val text = textFromPost(post, includeStory)
        TextPostSubject(text, from = post.from)
    }
  }

  /**
    * Extracts the text of a post
    * @param post post from which the text is extracted
    * @return the post's text
    */
  def textFromPost(post: FBPost, includeStory: Boolean = true): String = {
    post.message match {
      case Some(message) =>
        message + (if (includeStory) {
          post.story match {
            case Some(story) => "\n" + story
            case None => ""
          } 
        } else "")
      case None => post.story.getOrElse("")
    }
  }

  /**
    * Extracts the source of a media attachment
    * @param attachmentOpt media attachment option from which the source is extracted
    * @return the extracted source
    */
  def srcFromAttachments(attachmentOpt: Option[List[FBAttachment]]): Option[String] = {
    for {
      attachmentList <- attachmentOpt
      attachment <- attachmentList.headOption
      media <- attachment.media
    } yield media.src
  }

  /**
    * Extract a subject from a page
    * @param page page from which the subject must be extracted
    * @return a subject representing the page
    */
  def subjectFromPage(page: FBPage): PageSubject = {
    val url = page.photos match {
      case Some(p) => p.source
      case None => None
    }
    val name = page.name.getOrElse("")
    PageSubject(name, page.pageId, url)
  }
}

/**
  * Abstract question generator
  */
abstract class QuestionGenerator extends Actor with ActorLogging {
  override val log = Logging(context.system, this)
  /**
    * Get a number of document matching a query. Skips a random number of them.
    * @param db database containing the documents
    * @param collection collection containing the documents
    * @param query query to match
    * @param quantity quantity to get
    * @param reader implicit deserializer
    * @tparam T document type
    * @return a future list of documents
    */
  protected def getDocuments[T](db: DefaultDB,
                                collection: BSONCollection,
                                query: BSONDocument, quantity: Int)
                               (implicit reader: BSONDocumentReader[T]): Future[List[T]] = {
    val futureCount = collection.count(Some(query))
    futureCount.flatMap { count =>
      val skip = if (count - quantity > 0) Random.nextInt(count - quantity) else 0
      collection.find(query).
        options(QueryOpts(skipN = skip)).cursor[T]().collect[List](quantity)

    }
  }

  /**
    * Fetches all the posts matching the given ids and applies a function to them
    * @param postsCollection collection containing posts
    * @param userId user who made the posts (or was tagged in them)
    * @param postIds posts to fetch
    * @param client original requester
    * @param f function applied to the posts
    */
  protected def fetchPosts(postsCollection: BSONCollection, userId: String, postIds: List[String], client: ActorRef)(f: List[FBPost] => Unit): Unit = {
    val query = BSONDocument("userId" -> userId, "postId" -> BSONDocument("$in" -> postIds))
    postsCollection.find(query).cursor[FBPost]().collect[List]().onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Unknown error : $any.")
    }
  }

  /**
    * Fetches all the pages matching the given ids
    * @param pagesCollection collection containing pages
    * @param pageIds pages to fetch
    * @return a future list of pages
    */
  protected def fetchPages(pagesCollection: BSONCollection, pageIds: List[String]): Future[List[FBPage]] = {
    val query = BSONDocument("pageId" -> BSONDocument("$in" -> pageIds))
    pagesCollection.find(query).cursor[FBPage]().collect[List]()
  }

  /**
    * Fetches all the pages matching the given ids and applies a function to them, reports error to the client
    * @param pagesCollection collection containing pages
    * @param pageIds pages to fetch
    * @param client original requester
    * @param f function applied to the pages
    * @return a future list of page likes
    */
  def fetchPages(pagesCollection: BSONCollection, pageIds: List[String], client: ActorRef)(f: List[FBPage] => Unit): Unit = {
    fetchPages(pagesCollection, pageIds).onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Unknown error : $any.")
    }
  }

  /**
    * Fetches all the page likes matching the given ids
    * @param pageLikesCollection collection containing page likes
    * @param userId user who liked the pages
    * @param pageIds pages to fetch
    * @return a future list of page likes
    */
  protected def fetchLikedPages(pageLikesCollection: BSONCollection, userId: String,
                                pageIds: Option[List[String]]): Future[List[FBPageLike]] = {
    val query = pageIds match {
      case Some(list) =>
        BSONDocument("userId" -> userId, "pageId" -> BSONDocument("$in" -> list))
      case None =>
        BSONDocument("userId" -> userId)
    }
    pageLikesCollection.find(query).cursor[FBPageLike]().collect[List]()
  }

  /**
    * Fetches all the page likes matching the given ids and applies a function to them, reports error to the client
    * @param pageLikesCollection collection containing page likes
    * @param userId user who liked the pages
    * @param pageIds pages to fetch
    * @param client original requester
    * @param f function applied to the page likes
    */
  protected def fetchLikedPages(pageLikesCollection: BSONCollection, userId: String, client: ActorRef, pageIds: Option[List[String]] = None)
                               (f: List[FBPageLike] => Unit): Unit = {
    fetchLikedPages(pageLikesCollection, userId: String, pageIds).onComplete {
      case Success(list) => f(list)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Unknown error : $any.")
    }
  }

  /**
    * Fetches one page and applies a function to it, reports error to the client
    * @param pagesCollection collection containing pages
    * @param pageId page to fetch
    * @param client original requester
    * @param f function to apply to the page
    */
  protected def fetchPage(pagesCollection: BSONCollection, pageId: String, client: ActorRef)(f: Option[FBPage] => Unit): Unit = {
    val query = BSONDocument("pageId" -> pageId)
    pagesCollection.find(query).one[FBPage].onComplete {
      case Success(opt) => f(opt)
      case Failure(e) =>
        client ! MongoDBError(s"${e.getMessage}")
      case any =>
        client ! MongoDBError(s"Unknown error : $any.")
    }
  }
}
