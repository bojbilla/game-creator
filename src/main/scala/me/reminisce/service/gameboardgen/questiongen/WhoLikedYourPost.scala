package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPost, UserStats}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind.MultipleChoice
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType.MCWhoLikedYourPost
import me.reminisce.service.gameboardgen.GameboardEntities.{MultipleChoiceQuestion, Possibility}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator._
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Random, Success}


object WhoLikedYourPost {

  def props(database: DefaultDB): Props =
    Props(new WhoLikedYourPost(database))

}


class WhoLikedYourPost(database: DefaultDB) extends QuestionGenerator {

  def receive = {
    case CreateQuestion(userId, itemId) =>
      import scala.concurrent.ExecutionContext.Implicits.global
      val client = sender()
      // Note : if this question has been picked, it can only be if a UserStats exists

      val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      userCollection.find(BSONDocument("userId" -> userId)).one[UserStats].onComplete {
        case Success(userStatsOpt) =>
          userStatsOpt match {
            case Some(userStats) =>
              val postCollection = database[BSONCollection](MongoDatabaseService.fbPostsCollection)
              postCollection.find(BSONDocument("userId" -> userId, "postId" -> itemId)).one[FBPost].onComplete {
                case Success(postOpt) =>
                  postOpt match {
                    case Some(post) =>
                      val postSubject = subjectFromPost(post)
                      post.likes match {
                        case Some(likes) =>
                          val liker = Random.shuffle(likes).head
                          if ((userStats.likers -- likes.toSet).size < 3) {
                            client ! NotEnoughData(s"Not enough non likers for post $itemId and user $userId")
                          } else {
                            val choices = (liker :: Random.shuffle((userStats.likers -- likes.toSet).toList).take(3)) map {
                              choice => Possibility(choice.userName, None, "Person", Some(choice.userId))
                            }
                            val answer = choices.head
                            val shuffled = Random.shuffle(choices)
                            val gameQuestion = MultipleChoiceQuestion(userId, MultipleChoice, MCWhoLikedYourPost, Some(postSubject), shuffled, shuffled.indexOf(answer))
                            client ! FinishedQuestionCreation(gameQuestion)
                          }
                        case None =>
                          client ! NotEnoughData(s"No likes on post : $itemId") // this should be investigated
                      }

                    case None =>
                      client ! NotEnoughData(s"Post not found : $itemId") // this should be investigated
                  }
                case Failure(e) =>
                  client ! MongoDBError(s"${e.getMessage}")
              }
            case None =>
              client ! NotEnoughData(s"Strangely there is no userStats.")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any => log.error(s"WhoLikedYourPost received a unexpected message $any")
  }


}
