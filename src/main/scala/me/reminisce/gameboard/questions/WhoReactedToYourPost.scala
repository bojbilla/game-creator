package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.analysis.DataAnalyser
import me.reminisce.analysis.DataTypes._
import me.reminisce.database.AnalysisEntities.UserSummary
import me.reminisce.database.MongoCollections
import me.reminisce.database.MongoDBEntities.{AbstractReaction, FBPost, filterReaction}
import me.reminisce.gameboard.board.GameboardEntities._
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
    * Creates a WhoReactedToYourPostWithReactionType question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB, reactionType: ReactionType): Props =
  Props(new WhoReactedToYourPost(database, reactionType))

}


/**
  * WhoReactedToYourPost question generator
  *
  * @param db database from which to take the data
  */
class WhoReactedToYourPost(db: DefaultDB, reactionType: ReactionType) extends QuestionGenerator {

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
              postReactions = post.reactions.getOrElse(Set())
              reactionsWithComments = postReactions ++ post.commentsAsReactions
              blacklist = userSummary.blacklist.getOrElse(Set())
              filteredPostReactions = DataAnalyser.applyBlacklist(reactionsWithComments, blacklist)
              filteredReactioners = DataAnalyser.applyBlacklist(userSummary.reactioners, blacklist)
              selectedReaction <- randomReaction(filteredPostReactions)
              remainingReactions = reactionsDiff(filteredReactioners, filteredPostReactions)
              if remainingReactions.size >= 3
              choicesForDifficulty = getChoices(remainingReactions, userSummary.allReactionsCount, None)
              choices = (selectedReaction :: choicesForDifficulty) map {
                choice => Possibility(choice.from.userName, None, "Person", Some(choice.from.userId))
              }
              answer <- choices.headOption
              shuffled = Random.shuffle(choices)
              postSubject <- generateSubject(post, selectedReaction)
            }
              yield {
                MultipleChoiceQuestion(userId, MultipleChoice, reactionDataToQuestion(reactionType), Some(postSubject), shuffled, shuffled.indexOf(answer))
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

  /**
   * Get the choices for the question to be generated
   * 
   * @param reacts List of reactions
   * @param allReactionsCount Counter of reactions per reactioner
   * @param difficulty The difficulty of the question
   * @return 3 choices
   */
  private def getChoices(reacts: Set[AbstractReaction], allReactionsCount: Map[String, Int], difficulty: Option[Double]): List[AbstractReaction] = {
    difficulty match {
      case None => Random.shuffle(reacts.toList).take(3)
      case Some(d) => {
        val sortByReactionCount = reacts.map(r => r -> allReactionsCount(r.from.userId)).toList.sortBy(-_._2)
        val subset = sortByReactionCount.take(Math.max(3, ((4-sortByReactionCount.size)*d + sortByReactionCount.size).toInt))
        Random.shuffle(subset).take(3).map(_._1)
      }
    }
  }
  
  private val reactionDataToQuestion = Map[DataType, SpecificQuestionType](
    PostWhoReacted -> MCWhoReactedToYourPost,
    PostWhoLiked -> MCWhoReactedToYourPostWithLIKE,
    PostWhoWowed -> MCWhoReactedToYourPostWithWOW,
    PostWhoLaughed -> MCWhoReactedToYourPostWithHAHA,
    PostWhoLoved -> MCWhoReactedToYourPostWithLOVE,
    PostWhoGotSad -> MCWhoReactedToYourPostWithSAD,
    PostWhoGotAngry -> MCWhoReactedToYourPostWithANGRY,
    PostWhoCommented -> MCWhoMadeThisCommentOnYourPost
  )

  private def randomReaction[React <: AbstractReaction](reactions: Set[React]): Option[React] = {
    val possibleReactions = reactionType match {
      case PostWhoReacted => reactions
      case _ => filterReaction(reactions, reactionType)
    }
    Random.shuffle(possibleReactions).headOption
  }

  private def generateSubject(post: FBPost, selectedReaction: AbstractReaction): Option[Subject] = {
    val postSubject = subjectFromPost(post)
    reactionType match {
      case PostWhoCommented =>
        post.comments.flatMap {
          comments =>
            val maybeComment = Random.shuffle(comments.filter(_.from == selectedReaction.from)).headOption
            maybeComment.map {
              comment =>
                CommentSubject(comment.message, postSubject)
            }
        }
      case _ =>
        Some(postSubject)
    }
  }

  private def reactionsDiff[React <: AbstractReaction](totalReactions: Set[AbstractReaction], toRemove: Set[React]): Set[AbstractReaction] = {
    totalReactions.filterNot {
      elm =>
        toRemove.exists(_.from == elm.from)
    }
  }

}
