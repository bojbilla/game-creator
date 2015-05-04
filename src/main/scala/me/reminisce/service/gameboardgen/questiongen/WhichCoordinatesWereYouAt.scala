package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{CoordinatesQuestion, Location}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError}
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
    case CreateQuestion(userId, itemId) =>
      val client = sender()
      val query = BSONDocument(
        "userId" -> userId,
        "postId" -> itemId
      )
      val postCollection = db[BSONCollection](MongoDatabaseService.fbPostsCollection)
      postCollection.find(query).one[FBPost].onComplete {
        case Success(postOpt) =>
          val post = postOpt.get
          val postSubject = subjectFromPost(post)
          val location = Location(post.place.get.location.latitude, post.place.get.location.longitude)
          val gameQuestion = CoordinatesQuestion(userId, MultipleChoice, GeoWhatCoordinatesWereYouAt, postSubject, location)
          client ! FinishedQuestionCreation(gameQuestion)
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
      }
    case any =>
      log.error(s"Wrong message received $any.")
  }

}
