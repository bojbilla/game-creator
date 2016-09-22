package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.{FBComment, FBFrom, FBPost}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind._
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType._
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoMadeThisCommentOnYourPost]]
  */
object WhoMadeThisCommentOnYourPost {

  /**
    * Creates a WhoMadeThisCommentOnYourPost question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new WhoMadeThisCommentOnYourPost(database))
}

/**
  * WhoMadeThisCommentOnYourPost question generator
  * @param db database from which to take the data
  */
class WhoMadeThisCommentOnYourPost(db: DefaultDB) extends QuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
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

  /**
    * Gets comments made by 4 different people on the post
    * @param comments comments on the post
    * @return a set of candidate comments
    */
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

  /**
    * Generates a multiple choice question
    * @param userId user for which the question is meant
    * @param selectedComments comments from which the user will have to chose
    * @param rightOne answer to the question
    * @param post post about which the question is
    * @return a multiple choice question
    */
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


