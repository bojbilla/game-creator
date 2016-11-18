package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.gameboard.board.GameboardEntities.{MCWhoReactedToYourPost, MultipleChoice, MultipleChoiceQuestion, Possibility}
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoReactedToYourPost]]
  */
object WhoReactedToYourPost {

  /**
    * Creates a WhoReactedToYourPost question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhoReactedToYourPost(database))

}

/**
  * WhoReactedToYourPost question generator
  *
  * @param db database from which to take the data
  */
class WhoReactedToYourPost(db: DefaultDB) extends QuestionGenerator {

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
      // Note : if this question has been picked, it can only be if a UserSummary exists

      val userCollection = db[BSONCollection](MongoCollections.userSummaries)

      (for {
        maybeUserSummary <- userCollection.find(BSONDocument("userId" -> userId)).one[UserSummary]
        postCollection = db[BSONCollection](MongoCollections.fbPosts)
        maybePost <- postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost]
      }
        yield {
          val maybeQuestion =
            for {
              userSummary <- maybeUserSummary
              post <- maybePost
              likes <- post.reactions
              liker <- Random.shuffle(likes).headOption
              if !((userSummary.reactioners -- likes.toSet).size < 3)
              choices = (liker :: Random.shuffle((userSummary.reactioners -- likes.toSet).toList).take(3)) map {
                choice => Possibility(choice.from.userName, None, "Person", Some(choice.from.userId))
              }
              answer <- choices.headOption
              shuffled = Random.shuffle(choices)
              postSubject = subjectFromPost(post)
            }
              yield {
                MultipleChoiceQuestion(userId, MultipleChoice, MCWhoReactedToYourPost, Some(postSubject), shuffled, shuffled.indexOf(answer))
              }
          maybeQuestion match {
            case Some(question) =>
              client ! FinishedQuestionCreation(question)
            case None =>
              client ! NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough likers or non-likers.")
          }
        }) onFailure {
        case e =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any => log.error(s"WhoReactedToYourPost received a unexpected message $any")
  }


}
