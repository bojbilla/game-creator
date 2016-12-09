package me.reminisce.gameboard.questions.difficulty

import akka.actor.Props
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.{FBComment, FBFrom, FBPost}
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument
import me.reminisce.gameboard.questions._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}
import me.reminisce.database.AnalysisEntities.UserSummary

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoMadeThisCommentOnYourPostWithDifficulty]]
  */
object WhoMadeThisCommentOnYourPostWithDifficulty {

  /**
    * Creates a WhoMadeThisCommentOnYourPostWithDifficulty question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhoMadeThisCommentOnYourPostWithDifficulty(database))
}

/**
  * WhoMadeThisCommentOnYourPostWithDifficulty question generator
  *
  * @param db database from which to take the data
  */
class WhoMadeThisCommentOnYourPostWithDifficulty(db: DefaultDB) extends QuestionGenerator {

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
      val postCollection = db[BSONCollection](MongoCollections.fbPosts)
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
              val userCollection = db[BSONCollection](MongoCollections.userSummaries)
              for {
               maybeUserSummary <- userCollection.find(BSONDocument("userId" -> userId)).one[UserSummary]
              } yield {
                for{
                  userSummary <- maybeUserSummary
                } yield{
                  val bl = userSummary.blacklist.getOrElse(Set[FBFrom]())
                  val filteredComments = comments.filterNot { x => bl.contains(x.from) }
                  if (filteredComments.size < 4) 
                    NotEnoughData(s"Post has not enough comments : $itemId")
                  else
                    FinishedQuestionCreation(generateQuestionWithDifficulty(userId, filteredComments, post, None, userSummary)) 
                } 
              }
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
    *
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
    *
    * @param userId           user for which the question is meant
    * @param selectedComments comments from which the user will have to chose
    * @param rightOne         answer to the question
    * @param post             post about which the question is
    * @return a multiple choice question
    */
  private def generateQuestionWithDifficulty(userId: String, comments: List[FBComment],
                               post: FBPost, difficulty: Option[Double], userSummary: UserSummary): MultipleChoiceQuestion = {
    
    val commenters = comments.groupBy { c => c.from.userId }
    val ks = commenters.keySet
    val commentersCount = userSummary.commentersCommentsCount.filterKeys { ks.contains(_) }
    val notPostCommenters = userSummary.commentersCommentsCount -- commentersCount.keys
    
    // Take the answer from one of the fifth most commenters that have commented on the post
    val rightOne = commenters(Random.shuffle(commentersCount.toList.sortBy(-_._2).take(5)).head._1).head
    // The harder the question the more often commenters are picked
    val choices = notPostCommenters.toList.sortBy(-_._2).take(
        Math.max(3, ((4-notPostCommenters.size)*(difficulty.getOrElse(0.0)) + notPostCommenters.size).toInt))
    val choicesAsComments = choices.map(x => commenters(x._1).head)
    val shuffled = Random.shuffle(rightOne::choicesAsComments)
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


