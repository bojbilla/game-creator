package crawler.common

import spray.http.HttpResponse

object RemHelper {
  def accessTokenParam = "access_token"

  val since = "since"
  val until = "until"
  val accessToken = "access_token"
  val offset = "offset"
  val limit = "limit"
  val redirect = "redirect"
  val fields = "fields"

  implicit def uriWithParams(uri: String, params: Map[String, Any]): String ={
    val start = s"""$uri?${params.head._1}=${params.head._2}"""
    params.tail.foldLeft(start){
      case (acc, (k, v)) =>  s"""$acc&$k=${v.toString}"""
    }
  }

  def createResponseLog(response: HttpResponse): String = {
    s"""|Response for GET request
       |status : ${response.status.value}
        |headers: ${response.headers.mkString("\n  ", "\n  ", "")}
        |body   : ${response.entity.asString}""".stripMargin

  }
}
