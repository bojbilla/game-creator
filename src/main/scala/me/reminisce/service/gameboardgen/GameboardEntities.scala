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
                         photoURL: Option[String],
                         `type`: String = "Page") extends Subject(`type`)

  case class TextPostSubject(text: String, `type`: String = "TextPost") extends PostSubject(`type`, text)

  case class ImagePostSubject(text: String, imageUrl: Option[String], facebookImageUrl: Option[String],
                              `type`: String = "ImagePost") extends PostSubject(`type`, text)

  case class VideoPostSubject(text: String, thumbnailUrl: Option[String], url: Option[String],
                              `type`: String = "VideoPost") extends PostSubject(`type`, text)

  case class LinkPostSubject(text: String, thumbnailUrl: Option[String], url: Option[String],
                             `type`: String = "LinkPost") extends PostSubject(`type`, text)

  case class CommentSubject(comment: String, post: PostSubject, `type`: String = "Comment") extends Subject(`type`)


  abstract sealed class GameQuestion(userId: String, kind: QuestionKind, `type`: SpecificQuestionType, subject: Subject)

  case class MultipleChoiceQuestion(userId: String,
                                    kind: QuestionKind,
                                    `type`: SpecificQuestionType,
                                    subject: Subject,
                                    choices: List[Possibility],
                                    answer: Int) extends GameQuestion(userId, kind, `type`, subject)

  case class TimelineQuestion(userId: String,
                              kind: QuestionKind,
                              `type`: SpecificQuestionType,
                              subject: Subject,
                              answer: DateTime) extends GameQuestion(userId, kind, `type`, subject)


  case class Possibility(name: String, imageUrl: Option[String], fbId: Option[String] = None)

  case class CoordinatesQuestion(userId: String,
                                 kind: QuestionKind,
                                 `type`: SpecificQuestionType,
                                 subject: Subject,
                                 answer: Location) extends GameQuestion(userId, kind, `type`, subject)

  case class Location(latitude: Double, longitude: Double)

  case class Tile(`type`: QuestionKind,
                  question1: GameQuestion,
                  question2: GameQuestion,
                  question3: GameQuestion) extends RestMessage

  case class Board(userId: String, tiles: List[Tile], isTokenStale: Boolean) extends RestMessage

}
