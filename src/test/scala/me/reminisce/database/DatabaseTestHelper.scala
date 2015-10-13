package me.reminisce.database

import me.reminisce.TestsConfig

object DatabaseTestHelper {
  var portsInUse: Set[Int] = Set()

  def getNewPort: Int = {
    this.synchronized {
      while (portsInUse.size >= TestsConfig.maximumParallelEmbedMongoNumber) {
        this.wait()
      }
      val newPort = if (portsInUse.isEmpty) {
        27017
      } else {
        portsInUse.max + 1
      }
      portsInUse += newPort
      return newPort
    }
  }

  def releasePort(port: Int): Unit = {
    this.synchronized {
      if (portsInUse.contains(port)) {
        portsInUse -= port
        this.notifyAll()
      }
    }
  }
}