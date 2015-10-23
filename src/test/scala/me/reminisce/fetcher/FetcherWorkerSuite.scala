package me.reminisce.fetcher

import me.reminisce.fetcher.common.GraphResponses.Post
import org.scalatest.FunSuite

class FetcherWorkerSuite extends FunSuite {
  test("Test pruning") {
    val emptyPost = Post("id", from = None, message = None, story = None, place = None, likes = None, `type` = None,
      link = None, created_time = None, attachments = None, comments = None)
    val prunedPosts = FetcherWorker.prunePosts(Vector(emptyPost))
    assert(prunedPosts.isEmpty)
  }
}
