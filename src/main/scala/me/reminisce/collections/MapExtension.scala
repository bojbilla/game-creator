package me.reminisce.collections

object MapExtension {

  implicit final class ReversibleMap[T, U](val map: Map[T, List[U]]) {

    def reverse: Map[U, List[T]] = {
      (
        for {
          //DO NOT REMOVE .toIterable, no matter what IntelliJ says
          (key, values) <- map.toIterable
          value <- values
        } yield (value, key)
      ).groupBy(_._1).map(elm => (elm._1, elm._2.map(_._2).toList))
    }
  }

}
