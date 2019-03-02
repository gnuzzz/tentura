package ru.albemuth.tentura.performance

import org.slf4j.LoggerFactory
import ru.albemuth.tentura.tensor.{Matrix, NativeMatrix, TestUtils}
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object PerfMatrix2ScalarElementwise extends App with TestUtils {

  val LOGGER = LoggerFactory.getLogger(this.getClass)

  LOGGER.info("test started")

  //check
  val d1 = NativeMatrix.matrixData[Float](1001, 301)
  val m1 = Matrix.of(d1)
  val s = 11.2f
  val r = m1 + s
  val nativeR = (for (i <- d1.indices) yield {
    d1(i).map(v => v + s)
  }).toArray
  val maxError = compare(r.values(), nativeR)
  assert(maxError == 0)
  LOGGER.info("check complete")
  Memory.print("GPU memory usage")

  val data1 = NativeMatrix.matrixData[Float](100000, 3000)
  LOGGER.info("data created")

  Memory.print("GPU memory usage")
  val matrix1 = Matrix.of(data1)
  LOGGER.info("matrixes created")
  Memory.print("GPU memory usage")

  //warm up
  for (i <- 0 to 10) {
    val result = matrix1 + s
  }
  LOGGER.info("warmup complete")
  Memory.print("GPU memory usage")

  val stats = new Statistics("perf")
  for (attempt <- 0 to 100) {
    val t1 = System.nanoTime()
    for (i <- 0 to 10) {
      val result = matrix1 + s
    }
    val t2 = System.nanoTime()

    LOGGER.info(s"performance: ${(t2 - t1) / 10}")
    stats.addValue((t2 - t1) / 10)
  }
  LOGGER.info(s"average: ${stats.getAverage}")

}

//average: 1.8209910990099013E7
//average: 1.7631900198019803E7