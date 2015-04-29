package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{ActorRef, Props}
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{CoordinatesQuestion, Location, PostQuestion}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}


object WhichCoordinatesWereYouAt {

  def props(database: DefaultDB): Props =
    Props(new WhichCoordinatesWereYouAt(database))
}

class WhichCoordinatesWereYouAt(db: DefaultDB) extends QuestionGenerator {
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
          val location = Location(post.place.get.location.latitude, post.place.get.location.longitude)
          val postQuestion = PostQuestion(Geolocation, GeoWhatCoordinatesWereYouAt, postInQuestion, None)
          val gameQuestion = CoordinatesQuestion(user_id, postQuestion, location)
          client ! FinishedQuestionCreation(gameQuestion)
        case Failure(e) =>
          sendFailure(client, user_id)
      }
    case any =>
      log.error(s"Wrong message received $any.")
  }

  def sendFailure(client: ActorRef, user_id: String): Unit = {
    client ! FailedToCreateQuestion(s"Something went wrong WhichCoordinateWereYouAt user: $user_id", GeoWhichPlaceWereYouAt)
  }

}
