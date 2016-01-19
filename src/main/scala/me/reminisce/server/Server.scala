package me.reminisce.server

import java.util.concurrent.TimeUnit

import akka.actor.{ActorSystem, Props}
import akka.io.IO
import com.github.nscala_time.time.Imports._
import com.typesafe.config.ConfigFactory
import spray.can.Http
import sun.misc.{Signal, SignalHandler}

import scala.concurrent.ExecutionContext
import scala.concurrent.duration.Duration

/**
  * Application entry point.
  */
object Server extends App {
  // an ActorSystem to host our application in
  implicit val system = ActorSystem("server")
  implicit val context = ExecutionContext.Implicits.global
  val conf = ConfigFactory.load()

  val protocol = "http://"
  val hostName = ApplicationConfiguration.hostName
  val port = ApplicationConfiguration.serverPort
  // create and start our service actor
  val service = system.actorOf(Props[ServerServiceActor], "server-service")

  // sets the default timezone to UTC
  DateTimeZone.setDefault(DateTimeZone.forID("UTC"))

  // start a new HTTP server on port 9900 with our service actor as the handler
  IO(Http) ! Http.Bind(service, interface = hostName, port = port)

  // Handles the INT signal (unix signal 2, usually generated by a ^C) to shutdown gracefully
  Signal.handle(new Signal("INT"), new SignalHandler() {
    def handle(sig: Signal) {
      shutdown()
    }
  })

  // Handles the TERM signal (unix signal 15) to shutdown gracefully
  Signal.handle(new Signal("TERM"), new SignalHandler() {
    def handle(sig: Signal) {
      shutdown()
    }
  })

  /**
    * Shuts the system down in a proper way.
    */
  private def shutdown(): Unit = {
    println("System is shutting down...")
    IO(Http) ! Http.Unbind(Duration(10, TimeUnit.SECONDS))
    system.shutdown()
  }
}
