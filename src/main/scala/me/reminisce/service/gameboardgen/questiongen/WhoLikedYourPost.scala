package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.{FBPost, UserStat}
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
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
    case CreateQuestion(user_id, item_id) =>
      val client = sender()
      // Note : if this question has been picked, it can only be if a UserStat exists

      val userCollection = database[BSONCollection](MongoDatabaseService.userStatisticsCollection)

      userCollection.find(BSONDocument("user_id" -> user_id)).one[UserStat].onComplete {
        case Success(userStatOpt) =>
          userStatOpt match {
            case Some(userStat) =>
              val postCollection = database[BSONCollection](MongoDatabaseService.fbPostsCollection)
              postCollection.find(BSONDocument("user_id" -> user_id, "post_id" -> item_id)).one[FBPost].onComplete {
                case Success(postOpt) => {
                  val post = postOpt.get //post should exist
                  val postSubject = subjectFromPost(post)
                  val liker = Random.shuffle(post.likes.get).head
                  val choices = (liker :: Random.shuffle((userStat.likers -- post.likes.get.toSet).toList).take(3)) map {
                    choice => Possibility(choice.user_name, None, Some(choice.user_id))
                  }
                  val answer = choices.head
                  val shuffled = Random.shuffle(choices)
                  val gameQuestion = MultipleChoiceQuestion(user_id, MultipleChoice, MCWhoLikedYourPost, postSubject, shuffled, shuffled.indexOf(answer))
                  client ! FinishedQuestionCreation(gameQuestion)
                }
                case Failure(e) =>
                  client ! MongoDBError(s"${e.getMessage}")
              }
            case None =>
              client ! NotEnoughData(s"Strangely there is no userStat.")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }

    case any => log.error(s"WhoLikedYourPost received a unexpected message $any")
  }


}
