package me.reminisce.stats

import me.reminisce.gameboard.board.GameboardEntities.QuestionKind
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.QuestionKind

object StatsDataTypes {

  abstract class DataType(id: String) {
    val name: String = id
  }

  // Post only
  case object PostGeolocation extends DataType("PostGeolocation")

  case object PostWhoCommented extends DataType("PostWhoCommented")

  case object PostWhoLiked extends DataType("PostWhoLiked")

  case object PostCommentsNumber extends DataType("PostCommentsNumber")

  // Page Only
  case object PageWhichLiked extends DataType("PageWhichLiked")

  // Both
  case object LikeNumber extends DataType("LikeNumber")

  case object Time extends DataType("Time")


  def possibleTypes(questionKind: QuestionKind): List[DataType] = questionKind match {
    case QuestionKind.MultipleChoice =>
      List(PostWhoLiked, PostWhoCommented, PageWhichLiked)
    case QuestionKind.Timeline =>
      List(Time)
    case QuestionKind.Geolocation =>
      List(PostGeolocation)
    case QuestionKind.Order =>
      List(LikeNumber, PostCommentsNumber, Time)
    case _ =>
      List()
  }

  def possibleKind(dataType: DataType): List[QuestionKind] = dataType match {
    case Time =>
      List(QuestionKind.Timeline, QuestionKind.Order)
    case PostGeolocation =>
      List(QuestionKind.Geolocation)
    case PostWhoCommented =>
      List(QuestionKind.MultipleChoice)
    case PostWhoLiked =>
      List(QuestionKind.MultipleChoice)
    case PostCommentsNumber =>
      List(QuestionKind.Order)
    case PageWhichLiked =>
      List(QuestionKind.MultipleChoice)
    case LikeNumber =>
      List(QuestionKind.Order)
    case _ =>
      List()
  }


  def stringToType(typeName: String): DataType = typeName match {
    case PostGeolocation.name => PostGeolocation

    case PostWhoCommented.name => PostWhoCommented

    case PostWhoLiked.name => PostWhoLiked

    case PostCommentsNumber.name => PostCommentsNumber

    case PageWhichLiked.name => PageWhichLiked

    case LikeNumber.name => LikeNumber

    case Time.name => Time
  }

}
