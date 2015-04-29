package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{Actor, ActorContext, ActorLogging}
import me.reminisce.mongodb.MongoDBEntities.{FBAttachment, FBPost}
import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{GameQuestion, PostInQuestion}
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

  def postInQuestionFromPost(post: FBPost): PostInQuestion = {
    PostInQuestion(post.message.getOrElse("") + post.story.getOrElse(""), srcFromAttachments(post.attachments))
  }

}
