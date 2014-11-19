package entities

import entities.Entities.QuestionType.QuestionType
import org.joda.time.DateTime
import reactivemongo.bson.{BSONObjectID, BSONDocumentReader, BSONDocument, BSONDocumentWriter}
import server.domain.RestMessage


/**
 * Created by Aranir on 26/10/14.
 */
object Entities {

  object Status extends Enumeration {
    type Status = Value
    val New, Used = Value
  }

  object QuestionType extends Enumeration {
    type QuestionType = Value
    val MultipleChoice, Timeline, Geolocation, OrderedList = Value
  }

    sealed class GameQuestion
    case class Question(question:String, sub_question: Option[String], image_url: Option[String])

    case class MultipleChoiceQuestion( id: String,
                                       user_id: String,
                                       question: Question,
                                       choices: Vector[Possibility],
                                       answer: Int) extends GameQuestion{
      require(answer < choices.length)
      require(answer >= 0)
    }

    case class Possibility(text: Option[String], image_url: Option[String])
    case class TimelineQuestion(id: String, user_id: String, question: Question, answer: DateTime, min_date: Int, max_date: Int, range: Int) extends GameQuestion
    case class Geolocation(id: String, user_id: String, question: Question, answer: Location) extends GameQuestion
    case class Location(longitude: Double, latitude: Double)

    case class Tile(`type`: QuestionType, question1: GameQuestion, question2: GameQuestion, question3: GameQuestion)
    case class Board(user_id: String, tiles: List[Tile]) extends RestMessage

}
