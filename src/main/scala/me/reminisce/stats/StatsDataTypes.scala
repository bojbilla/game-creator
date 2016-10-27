package me.reminisce.stats

import me.reminisce.gameboard.board.GameboardEntities.QuestionKind
import me.reminisce.gameboard.board.GameboardEntities.QuestionKind.QuestionKind

/**
  * Defines the data types necessary to generate stats.
  */
object StatsDataTypes {

  abstract class DataType(id: String) {
    val name: String = id
  }

  // Post only
  case object PostGeolocation extends DataType("PostGeolocation")

  case object PostWhoCommented extends DataType("PostWhoCommented")

  case object PostWhoReacted extends DataType("PostWhoReacted")

  case object PostCommentsNumber extends DataType("PostCommentsNumber")

  // Page Only
  case object PageWhichLiked extends DataType("PageWhichLiked")

  // Both
  case object LikeNumber extends DataType("LikeNumber")

  case object Time extends DataType("Time")


  /**
    * Possible data types for a given question kind
    * @param questionKind question kind
    * @return list of data types
    */
  def possibleTypes(questionKind: QuestionKind): List[DataType] = questionKind match {
    case QuestionKind.MultipleChoice =>
      List(PostWhoReacted, PostWhoCommented, PageWhichLiked)
    case QuestionKind.Timeline =>
      List(Time)
    case QuestionKind.Geolocation =>
      List(PostGeolocation)
    case QuestionKind.Order =>
      List(LikeNumber, PostCommentsNumber, Time)
    case _ =>
      List()
  }

  /**
    * Possible question kinds for given data type
    * @param dataType data type
    * @return list of question kinds
    */
  def possibleKind(dataType: DataType): List[QuestionKind] = dataType match {
    case Time =>
      List(QuestionKind.Timeline, QuestionKind.Order)
    case PostGeolocation =>
      List(QuestionKind.Geolocation)
    case PostWhoCommented =>
      List(QuestionKind.MultipleChoice)
    case PostWhoReacted =>
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

  /**
    * Converts a string naming a data type to a DataType object
    * @param typeName type name as a string
    * @return a DataType object
    */
  def stringToType(typeName: String): DataType = typeName match {
    case PostGeolocation.name => PostGeolocation

    case PostWhoCommented.name => PostWhoCommented

    case PostWhoReacted.name => PostWhoReacted

    case PostCommentsNumber.name => PostCommentsNumber

    case PageWhichLiked.name => PageWhichLiked

    case LikeNumber.name => LikeNumber

    case Time.name => Time
  }

}
