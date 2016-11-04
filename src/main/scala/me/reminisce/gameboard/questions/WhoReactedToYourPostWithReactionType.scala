package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


/**
  * Factory for [[me.reminisce.gameboard.questions.WhoReactedToYourPostWithReactionType]]
  */
object WhoReactedToYourPostWithReactionType {

  /**
    * Creates a WhoReactedToYourPostWithReactionType question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB, reactionType: String): Props =
  Props(new WhoReactedToYourPostWithReactionType(database, reactionType))

}

/**
  * WhoReactedToYourPostWithReactionType question generator
  *
  * @param db database from which to take the data
  */
class WhoReactedToYourPostWithReactionType(db: DefaultDB, reactionType: String) extends QuestionGenerator {

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
        maybePost <- postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId, "reactions" -> BSONDocument("reactionType" -> reactionType))).one[FBPost]
      }
        yield {
          val maybeQuestion =
            for {
              userSummary <- maybeUserSummary
              post <- maybePost
              reactions <- post.reactions
              reactionerWithType <- Random.shuffle(reactions.filter {
                _.reactionType == reactionType
              }).headOption
              if !((userSummary.reactioners -- reactions.toSet).size < 3)
              choices = (reactionerWithType :: Random.shuffle((userSummary.reactioners -- reactions.toSet).toList).take(3)) map {
                choice => Possibility(choice.userName, None, "Person", Some(choice.userId))
              }
              answer <- choices.headOption
              shuffled = Random.shuffle(choices)
              postSubject = subjectFromPost(post)
            }
              yield {
                MultipleChoiceQuestion(userId, MultipleChoice, getSpecificQuestionType(reactionType), Some(postSubject), shuffled, shuffled.indexOf(answer))
              }
          maybeQuestion match {
            case Some(question) =>
              client ! FinishedQuestionCreation(question)
            case None =>
              client ! NotEnoughData(s"No user summary, $itemId does not exist or $itemId has not enough reactioners or non-reactioners.")
          }
        }) onFailure {
        case e =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any => log.error(s"WhoReactedToYourPostWithReactionType received a unexpected message $any")
  }

  private def getSpecificQuestionType(reactionType: String) = {
    Map(
      "LIKE" -> MCWhoReactedToYourPostWithLIKE,
      "WOW" -> MCWhoReactedToYourPostWithWOW,
      "HAHA" -> MCWhoReactedToYourPostWithHAHA,
      "LOVE" -> MCWhoReactedToYourPostWithLOVE,
      "SAD" -> MCWhoReactedToYourPostWithSAD,
      "ANGRY" -> MCWhoReactedToYourPostWithANGRY
    )(reactionType)
  }
}