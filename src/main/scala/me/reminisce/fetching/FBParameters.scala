package me.reminisce.fetching

import com.github.nscala_time.time.Imports._


case class FBParameters(access_token: Option[String] = None,
                        node: String = "",
                        fields: Option[String] = None,
                        since: DateTime = DateTime.now - 1.year,
                        until: DateTime = DateTime.now,
                        offset: Int = 0,
                        limit: Int = 100,
                        minimalEntities: Int = 1,
                        redirect: Boolean = false)

case class FBSimpleParameters(userId: Option[String] = None,
                              access_token: Option[String] = None,
                              query: Option[String] = None,
                              minimalEntities: Int = 0,
                              since: DateTime = DateTime.now - 1.year,
                              until: DateTime = DateTime.now) {
  def getSince = {
    (since.getMillis / 1000).toString
  }

  def getUntil = {
    (until.getMillis / 1000).toString
  }
}
