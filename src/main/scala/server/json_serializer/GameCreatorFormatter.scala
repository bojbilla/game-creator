package server.json_serializer

import entities.Entities.TileQuestionType
import org.json4s.ext.{EnumNameSerializer, JodaTimeSerializers}
import org.json4s.{DefaultFormats, Formats}

/**
 * Created by roger on 10/11/14.
 */
trait GameCreatorFormatter {
  implicit lazy val json4sFormats:Formats = DefaultFormats ++ JodaTimeSerializers.all +
  new EnumNameSerializer(TileQuestionType)
}
