package aws_testing

import asyncdynamo._
import scala.concurrent.duration._
import akka.util.Timeout
import org.scalatest.{Matchers, WordSpecLike}
import asyncdynamo.nonblocking.{Read, Save, CreateTable, TableExists}
import org.scalatest.FreeSpec

/**
 * Created by Aranir on 22/10/14.
 */

import asyncdynamo.nonblocking.{Read, Save, CreateTable, TableExists}


class CommonSpec extends FreeSpec with DynamoSupport
with Matchers {

  case class Person(id :String, name: String, email: String)
  implicit val personDO = DynamoObject.of3(Person)

      "Generates DO for basic case class" in {
        val tst = Person("12312321", "Piotr", "piotrga@gmail.com")
        assert(personDO.fromDynamo(personDO.toDynamo(tst)) == tst)
      }
  }