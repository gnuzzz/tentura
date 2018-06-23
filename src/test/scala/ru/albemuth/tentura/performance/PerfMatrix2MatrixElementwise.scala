package ru.albemuth.tentura.performance

import org.slf4j.LoggerFactory
import ru.albemuth.tentura.tensor.{Matrix, NativeMatrix, TestMatrixCasFunctions, TestUtils}
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object PerfMatrix2MatrixElementwise extends App with TestUtils {

  val LOGGER = LoggerFactory.getLogger(this.getClass)

  LOGGER.info("test started")

  //check
  val d1 = NativeMatrix.matrixData[Float](1001, 301)
  val d2 = NativeMatrix.matrixData[Float](1001, 301)
  val m1 = Matrix.of(d1)
  val m2 = Matrix.of(d2)
  val r = m1 + m2
  val nativeR = (for (i <- d1.indices) yield {
    d1(i).zip(d2(i)).map(v => v._1 + v._2)
  }).toArray
  val maxError = compare(r.values(), nativeR)
  assert(maxError == 0)
  LOGGER.info("check complete")
  Memory.print("GPU memory usage")

  val data1 = NativeMatrix.matrixData[Float](100000, 3000)
  val data2 = NativeMatrix.matrixData[Float](100000, 3000)
  LOGGER.info("data created")

  Memory.print("GPU memory usage")
  val matrix1 = Matrix.of(data1)
  val matrix2 = Matrix.of(data2)
  LOGGER.info("matrixes created")
  Memory.print("GPU memory usage")

  //warm up
  for (i <- 0 to 10) {
    val result = matrix1 + matrix2
  }
  LOGGER.info("warmup complete")
  Memory.print("GPU memory usage")

  val stats = new Statistics("perf")
  for (attempt <- 0 to 100) {
    val t1 = System.nanoTime()
    for (i <- 0 to 10) {
      val result = matrix1 + matrix2
    }
    val t2 = System.nanoTime()

    LOGGER.info(s"performance: ${(t2 - t1) / 10}")
    stats.addValue((t2 - t1) / 10)
  }
  LOGGER.info(s"average: ${stats.getAverage}")

}

//average: 2.5387158217821788E7
//average: 2.5407202673267327E7

//(2, 2, 2)       average: 201163883
//(4, 2, 512)     average:  26262641
//(8, 2, 64)      average:  25413485
//(1024, 8, 32)   average:  26308963
//(1024, 16, 32)  average:  26272297
//(1024, 32, 32)  average:  26720378