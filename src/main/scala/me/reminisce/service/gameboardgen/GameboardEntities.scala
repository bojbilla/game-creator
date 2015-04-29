package me.reminisce.service.gameboardgen

import me.reminisce.server.domain.RestMessage
import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind.QuestionKind
import me.reminisce.service.gameboardgen.GameboardEntities.SpecificQuestionType.SpecificQuestionType
import org.joda.time.DateTime

object GameboardEntities {

  object Status extends Enumeration {
    type Status = Value
    val New, Used = Value
  }

  object SpecificQuestionType extends Enumeration {
    type SpecificQuestionType = Value
    val TLWhenDidYouShareThisPost = Value("TLWhenDidYouShareThisPost")
    val GeoWhatCoordinatesWereYouAt = Value("GeoWhatCoordinatesWereYouAt")
    val GeoWhichPlaceWereYouAt = Value("GeoWhichPlaceWereYouAt")
    val MCWhoMadeThisCommentOnYourPost = Value("MCWhoMadeThisCommentOnYourPost")
    val MCWhichPageDidYouLike = Value("MCWhichPageDidYouLike")
    val MCWhoLikedYourPost = Value("MCWhoLikedYourPost")
  }

  object QuestionKind extends Enumeration {
    type QuestionKind = Value
    val MultipleChoice, Timeline, Geolocation, OrderedList = Value
  }


  abstract sealed class GameQuestion

  abstract sealed class Question(kind: QuestionKind, `type`: SpecificQuestionType)

  case class PostInQuestion(text: String, image_url: Option[String])

  case class PostQuestion(kind: QuestionKind,
                          `type`: SpecificQuestionType,
                          post: PostInQuestion, comment: Option[String]) extends Question(kind: QuestionKind, `type`: SpecificQuestionType)

  case class MCQuestion(kind: QuestionKind,
                        `type`: SpecificQuestionType) extends Question(kind: QuestionKind, `type`: SpecificQuestionType)

  case class Possibility(name: String, image_url: Option[String], fb_id: Option[String] = None)

  case class MultipleChoiceQuestion(user_id: String,
                                    question: Question,
                                    choices: List[Possibility],
                                    answer: Int) extends GameQuestion


  case class TimelineQuestion(user_id: String, question: Question, answer: DateTime) extends GameQuestion

  case class CoordinatesQuestion(user_id: String, question: Question, answer: Location) extends GameQuestion

  case class Location(latitude: Double, longitude: Double)

  case class PlaceQuestion(user_id: String, question: Question, answer: String) extends GameQuestion

  case class Tile(question1: GameQuestion, question2: GameQuestion, question3: GameQuestion) extends RestMessage

  case class Board(user_id: String, tiles: List[Tile], is_token_stale: Boolean) extends RestMessage

}
