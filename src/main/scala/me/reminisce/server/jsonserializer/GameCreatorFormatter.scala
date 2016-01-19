package me.reminisce.server.jsonserializer

import me.reminisce.gameboard.board.GameboardEntities.QuestionKind
import org.json4s.ext.{EnumNameSerializer, JodaTimeSerializers}
import org.json4s.{DefaultFormats, Formats}

/**
  * Defines the json serialization formats
  */
trait GameCreatorFormatter {
  implicit lazy val json4sFormats: Formats = DefaultFormats ++ JodaTimeSerializers.all +
    new EnumNameSerializer(QuestionKind)
}
