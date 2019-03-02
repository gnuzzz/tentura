package ru.albemuth.tentura.performance

import org.scalatest.Assertions
import org.slf4j.LoggerFactory
import ru.albemuth.tentura.tensor._
import ru.albemuth.tentura.tensor.kernel.matrix.Sum.TILE_DIM
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object PerfSumMatrix extends App with TestUtils with Assertions {

  val LOGGER = LoggerFactory.getLogger(this.getClass)

  LOGGER.info("test started")

  val d1 = NativeMatrix.matrixData[Float](1001, 301)
  val m1 = Matrix.of(d1)
  val r = Matrix.sum(m1)
  val nativeR = NativeVector.sumPar(d1.flatten, TILE_DIM)
  assert(r.value() === nativeR)
  LOGGER.info("check complete")
  Memory.print("GPU memory usage")

  val data1 = NativeMatrix.matrixData[Float](100000, 3000)
  LOGGER.info("data created")

  Memory.print("GPU memory usage")
  val matrix1 = Matrix.of(data1)
  LOGGER.info("matrix created")
  Memory.print("GPU memory usage")

  //warm up
  for (i <- 0 to 10) {
    val result = Matrix.sum(matrix1)
  }
  LOGGER.info("warmup complete")
  Memory.print("GPU memory usage")

  val stats = new Statistics("perf")
  for (attempt <- 0 to 100) {
    val t1 = System.nanoTime()
    for (i <- 0 to 10) {
      val result = Matrix.sum(matrix1)
    }
    val t2 = System.nanoTime()

    LOGGER.info(s"performance: ${(t2 - t1) / 10}")
    stats.addValue((t2 - t1) / 10)
  }
  LOGGER.info(s"average: ${stats.getAverage}")

}

//base - average: 1.2385784702970298E8
//256 -  average: 1.0776503534653465E8
//512 -  average: 5.824271574257426E7
//1024 - average: 3.3009768712871287E7