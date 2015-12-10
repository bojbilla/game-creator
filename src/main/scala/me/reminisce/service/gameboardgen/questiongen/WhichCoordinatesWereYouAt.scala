package me.reminisce.service.gameboardgen.questiongen

import akka.actor.Props
import me.reminisce.database.MongoDatabaseService
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind._
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{GeolocationQuestion, Location}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FinishedQuestionCreation, MongoDBError, NotEnoughData}
import reactivemongo.api.DefaultDB
import reactivemongo.api.collections.default.BSONCollection
import reactivemongo.bson.BSONDocument

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Random, Success}

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
          val questionOpt =
            for {
              post <- postOpt
              postSubject = QuestionGenerator.subjectFromPost(post)
              place <- post.place
            } yield {
              val answer = Location(place.location.latitude, place.location.longitude)
              //magic numbers
              val maxDisplacement = 0.03166666666
              val minDisplacement = 0.02743473384
              val t = Random.nextDouble() * (maxDisplacement - minDisplacement) + minDisplacement
              val theta = Random.nextDouble() * 2 * math.Pi
              val defaultLocation = Location(answer.latitude + t * math.sin(theta), answer.longitude + t * math.cos(theta))
              //magic number, around 2 kilometers
              val range = 0.02612831795
              GeolocationQuestion(userId, Geolocation, GeoWhatCoordinatesWereYouAt, Some(postSubject),
                answer, defaultLocation, range)
            }
          questionOpt match {
            case Some(q) =>
              client ! FinishedQuestionCreation(q)
            case None =>
              client ! NotEnoughData(s"Post has no place or post does not exist : $itemId")
          }
        case Failure(e) =>
          client ! MongoDBError(s"${e.getMessage}")
        case any =>
          client ! MongoDBError(s"$any")
      }
    case any =>
      log.error(s"Wrong message received $any.")
  }

}
