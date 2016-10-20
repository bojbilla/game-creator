package me.reminisce.gameboard.questions

import akka.actor.Props
import me.reminisce.database.MongoDBEntities.FBPost
import me.reminisce.database.MongoDatabaseService
import me.reminisce.database.StatsEntities.UserStats
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.MultipleChoice
import me.reminisce.gameboard.board.GameboardEntities.SpecificQuestionType.MCWhoLikedYourPost
import me.reminisce.gameboard.board.GameboardEntities.{MultipleChoiceQuestion, Possibility}
import me.reminisce.gameboard.questions.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.bson.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

/**
  * Factory for [[me.reminisce.gameboard.questions.WhoLikedYourPost]]
  */
object WhoLikedYourPost {

  /**
    * Creates a WhoLikedYourPost question generator
    * @param database database from which to take the data
    * @return props for the created actor
    */
  def props(database: DefaultDB): Props =
    Props(new WhoLikedYourPost(database))

}

/**
  * WhoLikedYourPost question generator
  * @param db database from which to take the data
  */
class WhoLikedYourPost(db: DefaultDB) extends QuestionGenerator {

  /**
    * Entry point for this actor, handles the CreateQuestionWithMultipleItems(userId, itemIds) message by getting the
    * necessary items from the database and creating a question. If some items are non conform to what is expected,
    * missing or there is an error while contacting the database, the error is reported to the client.
    * @return Nothing
    */
  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      // Note : if this question has been picked, it can only be if a UserStats exists

      val userCollection = db[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      (for {
        userStatsOpt <- userCollection.find(BSONDocument("userId" -> userId)).one[UserStats]
        postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
        postOpt <- postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost]
      }
        yield {
          println("----")
          println("userStatsOpt")
          println(userStatsOpt)
          println("postOpt")
          println(postOpt)
          val gameQuestionOpt =
            for {
              userStats <- userStatsOpt
              post <- postOpt
              likes <- post.reactions
              liker <- Random.shuffle(likes).headOption
              if !((userStats.likers -- likes.toSet).size < 3)
                choices = (liker :: Random.shuffle((userStats.likers -- likes.toSet).toList).take(3)) map {
                  choice => Possibility(choice.userName, None, "Person", Some(choice.userId))
                }
              answer <- choices.headOption
              shuffled = Random.shuffle(choices)
              postSubject = subjectFromPost(post)
            }
              yield {
                println("=============================")
                println(likes)
                println(liker)
                println(userStats.likers)
                println(choices)
                println(answer)
                println(postSubject)
                println("=============================")
                MultipleChoiceQuestion(userId, MultipleChoice, MCWhoLikedYourPost, Some(postSubject), shuffled, shuffled.indexOf(answer))
              }
          gameQuestionOpt match {
            case Some(q) =>
              client ! FinishedQuestionCreation(q)
            case None =>
              client ! NotEnoughData(s"No user stats, $itemId does not exist or $itemId has not enough likers or non-likers.")
          }
        }) onFailure {
        case e =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any => log.error(s"WhoLikedYourPost received a unexpected message $any")
  }


}
