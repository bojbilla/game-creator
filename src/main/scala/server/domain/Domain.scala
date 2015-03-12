package server.domain

/**
 * Created by roger on 09/11/14.
 */
trait RestMessage


// Domain objects

object Domain {

  case class Done(message: String) extends RestMessage

  //  case class MultipleChoiceQuestion(id: String, kind: String) extends RestMessage

  case class SliderQuestion(id: String, kind: String) extends RestMessage

  case class Error(message: String)

  case class TooManyRequests(message: String) extends RestMessage

  case class Validation(message: String)

  // Exceptions

  case object ExampleException extends Exception("Crawler encountered exception: need more power")

}