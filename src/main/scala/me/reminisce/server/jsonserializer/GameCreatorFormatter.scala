package me.reminisce.server.jsonserializer

import me.reminisce.gameboard.board.GameboardEntities._
import me.reminisce.server.jsonserializer.NamedClassSerialization.NamedClassSerializer
import org.json4s._
import org.json4s.ext.JodaTimeSerializers

/**
  * Defines the json serialization formats
  */
trait GameCreatorFormatter {

  implicit lazy val json4sFormats: Formats = DefaultFormats ++ JodaTimeSerializers.all +
    new NamedClassSerializer[QuestionKind](strToKind) + new NamedClassSerializer[SpecificQuestionType](strToType) +
    new NamedClassSerializer[TimeUnit](strToTimeUnit) + new NamedClassSerializer[SubjectType](strToSubjectType)
}
