package me.reminisce.gameboard.questions.difficulty

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
import me.reminisce.database.MongoDBEntities.FBReaction
import akka.actor.actorRef2Scala
import me.reminisce.gameboard.questions.QuestionGenerator
import reactivemongo.bson.Producer.nameValue2Producer
import me.reminisce.gameboard.questions._
import me.reminisce.database.MongoDBEntities.AbstractReaction

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoReactedToYourPostWithDifficulty]]
  */
object WhoReactedToYourPostWithDifficulty {
  /**
    * Creates a WhoReactedToYourPost question generator
    *
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
  Props(new WhoReactedToYourPostWithDifficulty(database))

}

/**
  * WhoReactedToYourPost question generator
  *
  * @param db database from which to take the data
  */
class WhoReactedToYourPostWithDifficulty(db: DefaultDB) extends QuestionGenerator {

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
      val difficulty = getDifficultyForQuestion(userId)
      (for {
        maybeUserSummary <- userCollection.find(BSONDocument("userId" -> userId)).one[UserSummary]
        postCollection = db[BSONCollection](MongoCollections.fbPosts)
        maybePost <- postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost]
      }
        yield {
          val maybeQuestion = generateQuestionWithDifficulty(difficulty, maybeUserSummary, maybePost)
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
  
  /**
   * Get the difficulty level for this user (win rate?)
   * 
   * @param userId The user id
   * @return the difficulty 
   */
  private def getDifficultyForQuestion(userId: String): Double = {
    return 0.5
  }
  
  /**
   * Generates a question with associated difficulty level
   * 
   * @param difficulty The difficulty of the question to be generated
   * @param maybeUserSummary The user summary
   * @param maybePost The post on which the question will be based
   * @return A multiple choice question
   */
  private def generateQuestionWithDifficulty(difficulty: Double, maybeUserSummary: Option[UserSummary], maybePost: Option[FBPost]): Option[MultipleChoiceQuestion] = {
    for{
      userSummary <- maybeUserSummary
      post <- maybePost
      reactions <- post.reactions
      if !((userSummary.reactioners -- reactions.toSet).size < 3)
      choices <- getChoices(reactions.toList, userSummary, difficulty)
      answer <- choices.headOption
      shuffled = Random.shuffle(choices)
      postSubject = subjectFromPost(post)  
    } yield {
        MultipleChoiceQuestion(userSummary.userId, MultipleChoice, MCWhoReactedToYourPost, Some(postSubject), shuffled, shuffled.indexOf(answer))
      }
  }
  
  /**
   * Get the choices for the question to be generated
   * 
   * @param reactions The list of reactioners on the fb post
   * @param userSummary User summary of the post owner
   * @param difficulty The difficulty of the question
   * @return List of Possibility
   */
  private def getChoices(reactions: List[AbstractReaction], userSummary: UserSummary, difficulty: Double): Option[List[Possibility]]= {
    val answer = getOftenReactioner(reactions.toSet, userSummary.reactionersReactionsCount)
    val choices = getReactionersAccordingDifficulty(userSummary.reactioners, userSummary.reactionersReactionsCount, reactions.toSet + answer, difficulty)
    Option((answer::Random.shuffle(choices)).map(choice => Possibility(choice.from.userName, None, "Person", Some(choice.from.userId))))
  }
  
  /**
   * Gets the {@code count} number of most often reactioners that reacted to the fbpost 
   * 
   * @param reactioners Set of all reactioners for the user
   * @param reactionersReactionsCount Map from reactioner to the number of total reactions
   * @param excluded The set of reactioner to exclude (the answer of the question and the reactioner of the post)
   */
  private def getOftenReactioner(reactioners: Set[AbstractReaction], reactionersReactionsCount: Map[String, Int], excluded: Set[AbstractReaction] = Set[AbstractReaction]()): AbstractReaction = {
    val filterTheExcluded =  reactioners -- excluded
    val sortByReactionCount = filterTheExcluded.map { react => react -> reactionersReactionsCount(react.from.userId) }.toList.sortBy(-_._2)
    sortByReactionCount(Random.nextInt(10))._1
  } 
  
  /**
   * Gets reactioners that reacted to the fbpost according to the difficulty level 
   * 
   * @param reactioners Set of all reactioners for the user
   * @param reactionersReactionsCount Map from reactioner to the number of total reactions
   * @param excluded The set of reactioner to exclude (the answer of the question and the reactioner of the post)
   */
  private def getReactionersAccordingDifficulty(reactioners: Set[AbstractReaction], reactionersReactionsCount: Map[String, Int], excluded: Set[AbstractReaction] = Set(), winRate: Double) : List[AbstractReaction] = {
    val filterTheExcluded =  reactioners -- excluded
    val sortByReactionCount = filterTheExcluded.map { x => x -> reactionersReactionsCount(x.from.userId) }.toList.sortBy(-_._2)
    val subset = sortByReactionCount.take(Math.max(3, sortByReactionCount.size/(winRate*10).toInt))
    Random.shuffle(subset).take(3).map(_._1)
  }
}
