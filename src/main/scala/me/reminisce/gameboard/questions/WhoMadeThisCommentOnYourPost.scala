package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.{FBComment, FBFrom, FBPost}
import me.reminisce.database.{MongoDBEntities, MongoDatabaseService}
import me.reminisce.gameboard.board.GameboardEntities
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

object WhoMadeThisCommentOnYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoMadeThisCommentOnYourPost(database))
}

class WhoMadeThisCommentOnYourPost(db: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost].onComplete {
        case Success(postOpt) =>
          (for {
            post <- postOpt
            comments <- post.comments
            selectedComments = getCandidatesComments(comments)
            rightOne <- selectedComments.headOption
          } yield {
            if (comments.size < 4) {
              NotEnoughData(s"Post has not enough comments : $itemId")
            } else {
              FinishedQuestionCreation(generateQuestion(userId, selectedComments, rightOne, post))
            }
          }) match {
            case Some(message) =>
              client ! message
            case None =>
              client ! NotEnoughData(s"Post '$itemId' not found or post has no comment.")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
        case any =>
          client ! MongoDBError(s"Unkown database error : $any.")
      }
    case any =>
      log.error(s"Unexpected message received : $any")
  }

  private def getCandidatesComments(comments: List[FBComment]): List[FBComment] = {
    val fromsSet: Set[FBFrom] = comments.map {
      comm => comm.from
    }.toSet

    val candidatesSet = Random.shuffle(fromsSet).take(4)
    val shuffledList = Random.shuffle(comments)
    candidatesSet.flatMap {
      elm =>
        shuffledList.find(comm => comm.from == elm)
    }.toList
  }

  private def generateQuestion(userId: String, selectedComments: List[FBComment],
                               rightOne: FBComment, post: FBPost): MultipleChoiceQuestion = {
    val shuffled = Random.shuffle(selectedComments)
    val answer = shuffled.indexOf(rightOne)
    val shuffledPossibilities = shuffled.map {
      comm => Possibility(comm.from.userName, None, "Person", Some(comm.from.userId))
    }
    val postSubject = QuestionGenerator.subjectFromPost(post)
    val commentSubject = CommentSubject(rightOne.message, postSubject)
    MultipleChoiceQuestion(userId, MultipleChoice,
      MCWhoMadeThisCommentOnYourPost, Some(commentSubject), shuffledPossibilities, answer)
  }

}


