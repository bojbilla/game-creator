package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBComment, FBFrom, FBPost}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
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
    case CreateQuestion(user_id, item_id) =>
      val client = sender()
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(BSONDocument("user_id" -> user_id, "post_id" -> item_id)).one[FBPost].onComplete {
        case Success(postOpt) =>
          val post = postOpt.get
          val selectedComments = getCandidatesComments(post.comments.get)
          val rightOne = selectedComments.head
          val shuffled = Random.shuffle(selectedComments)
          val answer = shuffled.indexOf(rightOne)
          val shuffledPossibilities = shuffled.map {
            comm => Possibility(comm.from.user_name, None, Some(comm.from.user_id))
          }
          val postSubject = subjectFromPost(post)
          val commentSubject = CommentSubject(rightOne.message, postSubject)
          val question = Question(MultipleChoice, MCWhoMadeThisCommentOnYourPost, commentSubject)
          val gameQuestion = MultipleChoiceQuestion(user_id, question, shuffledPossibilities, answer)
          client ! FinishedQuestionCreation(gameQuestion)
        case Failure(e) =>
          client ! FailedToCreateQuestion(s"Could not reach database : $e", MCWhoMadeThisCommentOnYourPost)
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


