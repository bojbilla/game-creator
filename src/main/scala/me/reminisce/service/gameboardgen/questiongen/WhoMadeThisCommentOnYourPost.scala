package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBComment, FBFrom, FBPost}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Random, Success}

object WhoMadeThisCommentOnYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoMadeThisCommentOnYourPost(database))
}

class WhoMadeThisCommentOnYourPost(db: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(userId, itemId) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val client = sender()
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost].onComplete {
        case Success(postOpt) =>
          postOpt match {
            case Some(post) =>
              post.comments match {
                case Some(comments) =>
                  if (comments.size < 4) {
                    client ! NotEnoughData(s"Post has not enough comments : $itemId")
                  } else {
                    val selectedComments = getCandidatesComments(comments)
                    val rightOne = selectedComments.head
                    val shuffled = Random.shuffle(selectedComments)
                    val answer = shuffled.indexOf(rightOne)
                    val shuffledPossibilities = shuffled.map {
                      comm => Possibility(comm.from.userName, None, "Person", Some(comm.from.userId))
                    }
                    val postSubject = QuestionGenerator.subjectFromPost(post)
                    val commentSubject = CommentSubject(rightOne.message, postSubject)
                    val gameQuestion = MultipleChoiceQuestion(userId, MultipleChoice, MCWhoMadeThisCommentOnYourPost, Some(commentSubject), shuffledPossibilities, answer)
                    client ! FinishedQuestionCreation(gameQuestion)
                  }
                case None =>
                  client ! NotEnoughData(s"Post has no comment : $itemId")
              }
            case None =>
              client ! NotEnoughData(s"Post not found : $itemId")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }
    case any =>
      log.error(s"Unexpected message received : $any")
  }

  def getCandidatesComments(comments: List[FBComment]): List[FBComment] = {
    val fromsSet: Set[FBFrom] = comments.map {
      comm => comm.from
    }.toSet

    val candidatesSet = Random.shuffle(fromsSet).take(4)
    val shuffledList = Random.shuffle(comments)
    candidatesSet.map {
      elm => shuffledList.filter(comm => comm.from == elm).head
    }.toList
  }

}


