package me.reminisce.service.stats

import me.reminisce.database.DatabaseTester
import org.scalatest.DoNotDiscover

@DoNotDiscover
class StatsHandlerSpec extends DatabaseTester("OrderByPageLikesSpec") {

  import scala.concurrent.ExecutionContext.Implicits.global

  "StatsHandler" must {
    "Save \"on the fly\" stats properly." in {

    }
  }

}
