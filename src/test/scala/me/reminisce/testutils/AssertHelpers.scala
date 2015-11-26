package me.reminisce.testutils

object AssertHelpers {

  def optionAssert[T](tested: Option[T])(assertions: (T => Boolean)*)(emptyCallback: () => Unit): Unit = tested match {
    case Some(t) =>
      assertions.foreach(assertion => assert(assertion(t)))
    case None =>
      emptyCallback()
  }

  def listHeadAssert[T](tested: List[T])(assertions: (T => Boolean)*)(emptyCallback: () => Unit): Unit = {
    optionAssert(tested.headOption)(assertions: _*)(emptyCallback)
  }
}
