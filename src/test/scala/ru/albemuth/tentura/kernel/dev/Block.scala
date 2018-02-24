package ru.albemuth.tentura.kernel.dev

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Block(val blockIdx: Idx, val blockDim: Dim) {
  var threadCounter = 0

  def run(kernel: (Idx, Dim, Idx) => Unit): Unit = {
    val threads = for (i <- 0 until blockDim.x; j <- 0 until blockDim.y; k <- 0 until blockDim.z) yield {
      val thread = new Thread(() => kernel(blockIdx, blockDim, Idx(i, j, k)))
      thread.start()
      thread
    }
    threads.foreach(_.join)
  }

  def syncThreads() {
    synchronized {
      threadCounter += 1
      if (threadCounter >= blockDim.x * blockDim.y * blockDim.z) {
        threadCounter = 0
        notifyAll()
      } else {
        wait()
      }
    }
  }
}