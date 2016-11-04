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
import me.reminisce.database.MongoDBEntities.FBReaction

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoReactedToYourPost]]
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
   * Get the difficulty level for this user
   * 
   * @param userId The user id
   * @return the difficulty 
   */
  private def getDifficultyForQuestion(userId: String): String = {
    return "EASY"
  }
  
  /**
   * Generates a question with associated difficulty level
   * 
   * @param difficulty The difficulty of the question to be generated
   * @param maybeUserSummary The user summary
   * @param maybePost The post on which the question will be based
   * @return A multiple choice question
   */
  private def generateQuestionWithDifficulty(difficulty: String, maybeUserSummary: Option[UserSummary], maybePost: Option[FBPost]): Option[MultipleChoiceQuestion] = {
    for{
      userSummary <- maybeUserSummary
      post <- maybePost
      reactions <- post.reactions
      if !((userSummary.reactioners -- reactions.toSet).size < 3)
      choices <- getChoices(reactions, userSummary, difficulty)
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
  private def getChoices(reactions: List[FBReaction], userSummary: UserSummary, difficulty: String): Option[List[Possibility]]= {
    val answer = getOftenReactioners(1, reactions.toSet, userSummary.reactionersReactionsCount).toSet
    val choices = funcSelector(difficulty)(3, userSummary.reactioners, userSummary.reactionersReactionsCount, answer++reactions)
    Option((answer.head::Random.shuffle(choices)).map(choice => Possibility(choice.userName, None, "Person", Some(choice.userId))))
  }
  
  /**
   * Selects the function to get the choices according to the difficulty level of the question
   * 
   * @param difficulty The difficulty of the question
   * @return The function that will generate the choices
   */
  private def funcSelector(difficulty: String) = {
    difficulty match {
      case "Easy" => getLeastOftenReactioners _
      case "Medium" => getMediumOftenReactioners _
      case "Hard" => getOftenReactioners _
    }                  
  }
  
  /**
   * Gets the {@code count} number of most often reactioners that reacted to the fbpost 
   * 
   * @param count The number of reactioner to return
   * @param reactioners Set of all reactioners for the user
   * @param reactionersReactionsCount Map from reactioner to the number of total reactions
   * @param excluded The set of reactioner to exclude (the answer of the question and the reactioner of the post)
   */
  private def getOftenReactioners(count: Int, reactioners: Set[FBReaction], reactionersReactionsCount: Map[String, Int], excluded: Set[FBReaction] = Set()): List[FBReaction] = {
    val filterTheExcluded =  reactioners -- excluded
    val sortByReactionCount = filterTheExcluded.map { react => react -> reactionersReactionsCount(react.userId) }.toList.sortBy(-_._2)
    sortByReactionCount.take(count).map(x => x._1)
  } 
  
  /**
   * Gets the {@code count} number of least often reactioners that reacted to the fbpost 
   * 
   * @param count The number of reactioner to return
   * @param reactioners Set of all reactioners for the user
   * @param reactionersReactionsCount Map from reactioner to the number of total reactions
   * @param excluded The set of reactioner to exclude (the answer of the question and the reactioner of the post)
   */
  private def getLeastOftenReactioners(count: Int, reactioners: Set[FBReaction], reactionersReactionsCount: Map[String, Int], excluded: Set[FBReaction] = Set()) : List[FBReaction] = {
    getOftenReactioners(count, reactioners, reactionersReactionsCount.mapValues(-_), excluded)
  }
  
  /**
   * Gets the {@code count} reactioners that reacted to the fb post. Those reactioners are picked randomly from the pool of
   * the first sixth of most often reactioners
   * 
   * @param count The number of reactioner to return
   * @param reactioners Set of all reactioners for the user
   * @param reactionersReactionsCount Map from reactioner to the number of total reactions
   * @param excluded The set of reactioner to exclude (the answer of the question and the reactioner of the post)
   */
  private def getMediumOftenReactioners(count: Int, reactioners: Set[FBReaction], reactionersReactionsCount: Map[String, Int], excluded: Set[FBReaction] = Set()) : List[FBReaction] = {
    val filterTheExcluded =  reactioners -- excluded
    val sortByReactionCount = filterTheExcluded.map { x => x -> reactionersReactionsCount(x.userId) }.toList.sortBy(-_._2)
    val sixthOfAll = sortByReactionCount.take(Math.max(count, sortByReactionCount.size/6))
    Random.shuffle(sixthOfAll).take(count).map(_._1)
  }
}
