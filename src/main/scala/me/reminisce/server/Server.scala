package me.reminisce.server

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.github.nscala_time.time.Imports._
import com.typesafe.config.ConfigFactory
import me.reminisce.service.ApplicationConfiguration
import spray.can.Http

import scala.concurrent.ExecutionContext

object Server extends App {
  // we Server an ActorSystem to host our application in
  implicit val system = ActorSystem("server")
  implicit val context = ExecutionContext.Implicits.global
  val conf = ConfigFactory.load()

  val protocol = "http://"
  //make sure the following env variables are defined on your system
  val hostName = ApplicationConfiguration.hostName
  val port = ApplicationConfiguration.serverPort
  // create and start our service actor
  val service = system.actorOf(Props[ServerServiceActor], "server-service")

  // sets the default timezone to UTC
  DateTimeZone.setDefault(DateTimeZone.forID("UTC"))

  //  implicit val timeout = Timeout(5.seconds)
  // start a new HTTP server on port 9900 with our service actor as the handler

  IO(Http) ! Http.Bind(service, interface = hostName, port = port)
}
