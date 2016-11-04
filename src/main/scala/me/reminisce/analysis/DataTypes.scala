package me.reminisce.analysis

import me.reminisce.gameboard.board.GameboardEntities._

/**
  * Defines the data types necessary to generate the user summaries.
  */
object DataTypes {

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
    *
    * @param questionKind question kind
    * @return list of data types
    */
  def possibleTypes(questionKind: QuestionKind): List[DataType] = questionKind match {
    case MultipleChoice =>
      List(PostWhoReacted, PostWhoCommented, PageWhichLiked)
    case Timeline =>
      List(Time)
    case Geolocation =>
      List(PostGeolocation)
    case Order =>
      List(LikeNumber, PostCommentsNumber, Time)
    case _ =>
      List()
  }

  /**
    * Possible question kinds for given data type
    *
    * @param dataType data type
    * @return list of question kinds
    */
  def possibleKind(dataType: DataType): List[QuestionKind] = dataType match {
    case Time =>
      List(Timeline, Order)
    case PostGeolocation =>
      List(Geolocation)
    case PostWhoCommented =>
      List(MultipleChoice)
    case PostWhoReacted =>
      List(MultipleChoice)
    case PostCommentsNumber =>
      List(Order)
    case PageWhichLiked =>
      List(MultipleChoice)
    case LikeNumber =>
      List(Order)
    case _ =>
      List()
  }

  /**
    * Converts a string naming a data type to a DataType object
    *
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
