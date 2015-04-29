package me.reminisce.service.gameboardgen.questiongen

import akka.actor.{ActorRef, Props}
import me.reminisce.mongodb.MongoDBEntities.FBPost
import me.reminisce.service.gameboardgen.GameboardEntities
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType._
import me.reminisce.service.gameboardgen.GameboardEntities.{PlaceQuestion, Question}
import me.reminisce.service.gameboardgen.questiongen.QuestionGenerator.{CreateQuestion, FailedToCreateQuestion, FinishedQuestionCreation}
import reactivemongo.api.DefaultDB
import reactivemongo.bson.BSONDocument

import scala.util.{Failure, Success}


object WhichPlaceWereYouAt {

  def props(database: DefaultDB): Props =
    Props(new WhichPlaceWereYouAt(database))
}

class WhichPlaceWereYouAt(db: DefaultDB) extends PostQuestionGenerator(db) {
  def receive = {
    case CreateQuestion(user_id) =>
      val client = sender()
      val query = BSONDocument(
        "user_id" -> user_id,
        "place" -> BSONDocument("$exists" -> "true")
      )
      getDocument(db, collection, query).onComplete {
        case Success(optPost) => optPost match {
          case Some(post: FBPost) =>
            post.place match {
              case Some(place) =>
                val question = PlaceQuestion(post.post_id, user_id, Question("WhichPlaceWereYouAt",
                  Some(List(post.message.getOrElse(""), post.story.getOrElse("")))), place.name)
                client ! FinishedQuestionCreation(question)
              case None => sendFailure(client, user_id)
            }
          case None => sendFailure(client, user_id)
        }
        case Failure(e) => sendFailure(client, user_id)
      }
  }

  def sendFailure(client: ActorRef, user_id: String): Unit = {
    client ! FailedToCreateQuestion(s"Something went wrong WhichPlaceWereYouAt user: $user_id", GeoWhichPlaceWereYouAt)
  }

}
