package ru.albemuth.tentura.performance

import org.scalatest.Assertions
import org.slf4j.LoggerFactory
import ru.albemuth.tentura.tensor.{Matrix, NativeMatrix, TestUtils}
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object PerfMatrixDotMatrix extends App with TestUtils with Assertions {

  val LOGGER = LoggerFactory.getLogger(this.getClass)

  LOGGER.info("test started")

  val d1 = NativeMatrix.matrixData[Float](1001, 301)
  val d2 = NativeMatrix.matrixData[Float](301, 1001)
  val m1 = Matrix.of(d1)
  val m2 = Matrix.of(d2)
  val r = m1 *** m2
  val nativeM1 = NativeMatrix(d1)
  val nativeM2 = NativeMatrix(d2)
  val nativeR = nativeM1 *** nativeM2
  val maxError = compare(r.values(), nativeR.data)
  LOGGER.info("check complete")
  Memory.print("GPU memory usage")

  val data1 = NativeMatrix.matrixData[Float](200, 3073)
  val data2 = NativeMatrix.matrixData[Float](3073, 10)
//  val data1 = NativeMatrix.matrixData[Float](20000, 1000)
//  val data2 = NativeMatrix.matrixData[Float](1000, 20000)

  LOGGER.info("data created")

  Memory.print("GPU memory usage")
  val matrix1 = Matrix.of(data1)
  val matrix2 = Matrix.of(data2)
  LOGGER.info("matrix created")
  Memory.print("GPU memory usage")

  //warm up
  for (i <- 0 to 10) {
    val result = matrix1 *** matrix2
  }
  LOGGER.info("warmup complete")
  Memory.print("GPU memory usage")

  val stats = new Statistics("perf")
  for (attempt <- 0 to 100) {
    val t1 = System.nanoTime()
    for (i <- 0 to 10) {
      val result = matrix1 *** matrix2
    }
    val t2 = System.nanoTime()

    LOGGER.info(s"performance: ${(t2 - t1) / 10}")
    stats.addValue((t2 - t1) / 10)
  }
  LOGGER.info(s"average: ${stats.getAverage}")

}

//base    - (200, 3073) x (3073, 10) - average: 311494.1584158415
//dynamic - (200, 3073) x (3073, 10) - average: 371852.37623762386
//select  - (200, 3073) x (3073, 10) - average: 272175.8415841585
//base    - (20000, 1400) x (1400, 20000) - average: 1.606293915E9
//select  - (20000, 1400) x (1400, 20000) - average: 1.606293915E9