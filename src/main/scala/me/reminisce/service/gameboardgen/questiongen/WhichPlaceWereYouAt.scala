package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{ActorRef, Props}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities._
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}


object WhichPlaceWereYouAt {

  def props(database: DefaultDB): Props =
    Props(new WhichPlaceWereYouAt(database))
}

class WhichPlaceWereYouAt(db: DefaultDB) extends QuestionGenerator {
  def receive = {
    case CreateQuestion(user_id, item_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "post_id" -> item_id
      )
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(query).one[FBPost].onComplete {
        case Success(postOpt) =>
          val post = postOpt.get
          val postInQuestion = postInQuestionFromPost(post)
          val place = post.place.get.name
          val postQuestion = PostQuestion(Geolocation, GeoWhatCoordinatesWereYouAt, postInQuestion, None)
          val gameQuestion = PlaceQuestion(user_id, postQuestion, place)
          client ! FinishedQuestionCreation(gameQuestion)
        case Failure(e) =>
          sendFailure(client, user_id)
      }
    case any =>
      log.error(s"Wrong message type received $any.")
  }

  def sendFailure(client: ActorRef, user_id: String): Unit = {
    client ! FailedToCreateQuestion(s"Something went wrong WhichPlaceWereYouAt user: $user_id", GeoWhichPlaceWereYouAt)
  }

}
