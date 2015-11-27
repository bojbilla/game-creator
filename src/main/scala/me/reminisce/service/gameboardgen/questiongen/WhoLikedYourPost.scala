package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.mongodb.StatsEntities.UserStats
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind.MultipleChoice
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType.MCWhoLikedYourPost
import me.reminisce.service.gameboardgen.GameboardEntities.{MultipleChoiceQuestion, Possibility}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random


object WhoLikedYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoLikedYourPost(database))

}


class WhoLikedYourPost(database: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      // Note : if this question has been picked, it can only be if a UserStats exists

      val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      (for {
        userStatsOpt <- userCollection.find(BSONDocument("userId" -> userId)).one[UserStats]
        postCollection = database[BSONCollection](MongoDatabaseService.fbPostsCollection)
        postOpt <- postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost]
      }
        yield {
          val gameQuestionOpt =
            for {
              userStats <- userStatsOpt
              post <- postOpt
              likes <- post.likes
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
