package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.mongodb.MongoDBEntities.{FBAttachment, FBPost}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType.SpecificQuestionType
import me.reminisce.service.gameboardgen.GameboardEntities._
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.api.{DefaultDB, QueryOpts}
import reactivemongo.bson.{BSONDocument, BSONDocumentReader}
import reactivemongo.core.commands.Count

import scala.concurrent.{ExecutionContextExecutor, Future}
import scala.util.Random

object QuestionGenerator {

  case class CreateQuestion(user_id: String, item_id: String) extends RestMessage

  case class FinishedQuestionCreation(question: GameQuestion)

  case class FailedToCreateQuestion(message: String, questionType: SpecificQuestionType) extends RestMessage

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
            val image_url = srcFromAttachments(post.attachments)
            val facebook_image_url = post.link
            ImagePostSubject(text, image_url, facebook_image_url)
          case "video" =>
            val text = textFromPost(post)
            val thumbnail_url = srcFromAttachments(post.attachments)
            val url = post.link
            VideoPostSubject(text, thumbnail_url, url)
          case "link" =>
            val text = textFromPost(post)
            val thumbnail_url = srcFromAttachments(post.attachments)
            val url = post.link
            LinkPostSubject(text, thumbnail_url, url)
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
