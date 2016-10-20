package me.reminisce.fetching

import me.reminisce.fetching.config.GraphResponses.Post
import org.scalatest.FunSuite

class FetcherWorkerSuite extends FunSuite {
  test("Test pruning.") {
    val emptyPost = Post("id", from = None, message = None, story = None, place = None, reactions = None, `type` = None,
      link = None, created_time = None, attachments = None, comments = None)
    val prunedPosts = FetcherWorker.prunePosts(Vector(emptyPost))
    assert(prunedPosts.isEmpty)
  }
}
