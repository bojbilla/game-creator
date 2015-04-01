package entities

import entities.Entities.QuestionType.QuestionType
import org.joda.time.DateTime
import server.domain.RestMessage


/**
 * Created by Aranir on 26/10/14.
 */
object Entities {

  object Status extends Enumeration {
    type Status = Value
    val New, Used = Value
  }

  object SpecificQuestionType extends Enumeration {
    type SpecificQuestionType = Value
    val MCWhoLikedYourPost, MCWhichPageDidYouLike, MCWhoMadeThisCommentOnYourPost,
    TLWhenDidYouShareThisPost = Value
  }

  object QuestionType extends Enumeration {
    type QuestionType = Value
    val MultipleChoice, Timeline, Geolocation, OrderedList = Value
  }


    abstract sealed class GameQuestion {
      val id: String
    }
    case class Question(question:String, text: Option[List[String]] = Some(List("")), image_url: Option[String] = Some(""))

    case class MultipleChoiceQuestion(id: String,
                                       user_id: String,
                                       question: Question,
                                       choices: Vector[Possibility],
                                       answer: Int) extends GameQuestion{
      require(answer < choices.length)
      require(answer >= 0)
    }

    case class Possibility(text: Option[String] = Some(""), image_url: Option[String] = Some(""), fb_id: Option[String] = Some(""))
    case class TimelineQuestion(id: String, user_id: String, question: Question, min_date: Int,
                                max_date: Int, range: Int, answer: DateTime) extends GameQuestion
    case class GeolocationQuestion(id: String, user_id: String, question: Question, answer: Location) extends GameQuestion
    case class Location(longitude: Double, latitude: Double)

    case class Tile(`type`: QuestionType, question1: GameQuestion, question2: GameQuestion, question3: GameQuestion) extends RestMessage
    case class Board(user_id: String, tiles: List[Tile]) extends RestMessage

}
