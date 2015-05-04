package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.mongodb.MongoDBEntities.{FBAttachment, FBPost}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Random

object QuestionGenerator {

  case class CreateQuestion(userId: String, itemId: String) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class MongoDBError(message: String)

  case class NotEnoughData(message: String)

}

abstract class QuestionGenerator extends Actor with ActorLogging {
  implicit def dispatcher: ExecutionContextExecutor = context.dispatcher

  implicit def actorRefFactory: ActorContext = context

  def getDocuments[T](db: DefaultDB,
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

  def srcFromAttachments(attachmentOpt: Option[List[FBAttachment]]): Option[String] = {
    attachmentOpt.flatMap {
      attachmentList =>
        attachmentList.head.media.map {
          m => m.src
        }
    }
  }

  def textFromPost(post: FBPost): String = {
    post.message.getOrElse("") + "\n" + post.story.getOrElse("")
  }

  def subjectFromPost(post: FBPost): PostSubject = {
    post.`type` match {
      case Some(tpe) =>
        tpe match {
          case "photo" =>
            val text = textFromPost(post)
            val imageUrl = srcFromAttachments(post.attachments)
            val facebook_image_url = post.link
            ImagePostSubject(text, imageUrl, facebook_image_url)
          case "video" =>
            val text = textFromPost(post)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            VideoPostSubject(text, thumbnailUrl, url)
          case "link" =>
            val text = textFromPost(post)
            val thumbnailUrl = srcFromAttachments(post.attachments)
            val url = post.link
            LinkPostSubject(text, thumbnailUrl, url)
          case _ =>
            val text = textFromPost(post)
            TextPostSubject(text)
        }
      case None =>
        val text = textFromPost(post)
        TextPostSubject(text)
    }
  }

}
