package baseTesting

import akka.actor.{ActorLogging, ActorSystem}
import akka.testkit.{ImplicitSender, TestKit}
import org.scalatest.{BeforeAndAfterAll, Matchers, WordSpecLike}

abstract class BaseTesting extends TestKit(ActorSystem("testSystem"))
with WordSpecLike
with Matchers
with ImplicitSender
with BeforeAndAfterAll
