package me.reminisce.server.jsonserializer

import me.reminisce.service.gameboardgen.GameboardEntities.QuestionKind
import org.json4s.ext.{EnumNameSerializer, JodaTimeSerializers}
import org.json4s.{DefaultFormats, Formats}

/**
 * Created by roger on 10/11/14.
 */
trait GameCreatorFormatter {
  implicit lazy val json4sFormats: Formats = DefaultFormats ++ JodaTimeSerializers.all +
    new EnumNameSerializer(QuestionKind)
}
