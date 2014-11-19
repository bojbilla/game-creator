package server

import akka.actor.{Props, ActorSystem}
import akka.io.IO
import com.typesafe.config.ConfigFactory
import server.ServerServiceActor
import spray.can.Http

import scala.concurrent.ExecutionContext
import scala.util.Properties._

/**
 * Created by roger on 10/11/14.
 */
object Server extends App {
  // we Server an ActorSystem to host our application in
  implicit val system = ActorSystem("server")
  implicit val context = ExecutionContext.Implicits.global
  val conf = ConfigFactory.load()

  val protocol = "http://"
  //make sure the following env variables are defined on your system
  //$PAY_ME_BACK_HOST="localhost"  and  $PAY_ME_BACK_PORT="9000"
  val hostName = envOrElse("GAME_CREATOR_HOST", "localhost")
  val port = 9900

  // create and start our service actor
  val service = system.actorOf(Props[ServerServiceActor], "server-service")

  //  implicit val timeout = Timeout(5.seconds)
  // start a new HTTP server on port 8080 with our service actor as the handler

  IO(Http) ! Http.Bind(service, interface = hostName, port = port)
}
