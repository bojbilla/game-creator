package me.reminisce.server.jsonserializer

import org.json4s.CustomSerializer
import org.json4s.JsonAST.JString

object NamedClassSerialization {

  abstract class NamedCaseClass {
    val name: String
  }

  class NamedClassSerializer[E <: NamedCaseClass](converter: String => E)(implicit m: Manifest[E]) extends CustomSerializer[E](implicit formats => ( {
    case kind: JString =>
      converter(kind.values)
  }, {
    case kind: E =>
      JString(kind.name)
  }))

}