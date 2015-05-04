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
    val MCWhoMadeThisCommentOnYourPost = Value("MCWhoMadeThisCommentOnYourPost")
    val MCWhichPageDidYouLike = Value("MCWhichPageDidYouLike")
    val MCWhoLikedYourPost = Value("MCWhoLikedYourPost")
  }

  object QuestionKind extends Enumeration {
    type QuestionKind = Value
    val MultipleChoice, Timeline, Geolocation, OrderedList, Misc = Value
  }


  abstract sealed class Subject(`type`: String)

  abstract sealed class PostSubject(`type`: String, text: String) extends Subject(`type`)

  case class PageSubject(name: String, url: String,
                         photo_url: Option[String],
                         `type`: String = "Page") extends Subject(`type`)

  case class TextPostSubject(text: String, `type`: String = "TextPost") extends PostSubject(`type`, text)

  case class ImagePostSubject(text: String, image_url: Option[String], facebook_image_url: Option[String],
                              `type`: String = "ImagePost") extends PostSubject(`type`, text)

  case class VideoPostSubject(text: String, thumbnail_url: Option[String], url: Option[String],
                              `type`: String = "VideoPost") extends PostSubject(`type`, text)

  case class LinkPostSubject(text: String, thumbnail_url: Option[String], url: Option[String],
                             `type`: String = "LinkPost") extends PostSubject(`type`, text)

  case class CommentSubject(comment: String, post: PostSubject, `type`: String = "Comment") extends Subject(`type`)


  case class Question(kind: QuestionKind, `type`: SpecificQuestionType, subject: Subject)

  abstract sealed class GameQuestion(user_id: String, question: Question)

  case class MultipleChoiceQuestion(user_id: String,
                                    question: Question,
                                    choices: List[Possibility],
                                    answer: Int) extends GameQuestion(user_id, question)

  case class TimelineQuestion(user_id: String,
                              question: Question,
                              answer: DateTime) extends GameQuestion(user_id, question)


  case class Possibility(name: String, image_url: Option[String], fb_id: Option[String] = None)

  case class CoordinatesQuestion(user_id: String,
                                 question: Question,
                                 answer: Location) extends GameQuestion(user_id, question)

  case class Location(latitude: Double, longitude: Double)

  case class Tile(`type`: QuestionKind,
                  question1: GameQuestion,
                  question2: GameQuestion,
                  question3: GameQuestion) extends RestMessage

  case class Board(user_id: String, tiles: List[Tile], is_token_stale: Boolean) extends RestMessage

}
