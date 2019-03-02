package ru.albemuth.tentura.performance

import org.scalatest.Assertions
import org.slf4j.LoggerFactory
import ru.albemuth.tentura.tensor.kernel.matrix.SumColumns
import ru.albemuth.tentura.tensor._
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object PerfSumColumns extends App with TestUtils with Assertions {

  val LOGGER = LoggerFactory.getLogger(this.getClass)

  LOGGER.info("test started")

  val d1 = NativeMatrix.matrixData[Float](1001, 301)
  val m1 = Matrix.of(d1)
  val r = Matrix.sum(m1, axis = 1)
  val nativeR = d1.map(NativeVector.sumPar(_, SumColumns.TILE_DIM))
  val maxError = compare(r.values(), nativeR)
  assert(maxError === 0)
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
    val result = Matrix.sum(matrix1, axis = 1)
  }
  LOGGER.info("warmup complete")
  Memory.print("GPU memory usage")

  val stats = new Statistics("perf")
  for (attempt <- 0 to 100) {
    val t1 = System.nanoTime()
    for (i <- 0 to 10) {
      val result = Matrix.sum(matrix1, axis = 1)
    }
    val t2 = System.nanoTime()

    LOGGER.info(s"performance: ${(t2 - t1) / 10}")
    stats.addValue((t2 - t1) / 10)
  }
  LOGGER.info(s"average: ${stats.getAverage}")

}

//average: 9413628.514851488
//average: 8328198.118811881

//16 -  average: 9318016.831683166
//32 -  average: 8331919.306930693
//64 -  average: 8241821.386138614
//128 - average: 8226539.702970297
//256 - average: 9669504.950495047