package ru.albemuth.tentura.performance

import org.slf4j.{Logger, LoggerFactory}
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor._
import ru.albemuth.tentura.util.Memory
import ru.albemuth.util.Statistics

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class PerfMatrixCasFunctions extends /*FunSuite with */TestUtils with TestWithResult {

  def calculate(value: Float, comparator: Comparator, threshold: Float, operand: Float): Float = {
    comparator match {
      case Comparator.== => if (value == threshold) operand else value
      case Comparator.!= => if (value != threshold) operand else value
      case Comparator.< =>  if (value < threshold) operand else value
      case Comparator.<= => if (value <= threshold) operand else value
      case Comparator.> =>  if (value > threshold) operand else value
      case Comparator.>= => if (value >= threshold) operand else value
    }
  }

  val LOGGER: Logger = LoggerFactory.getLogger(classOf[TestMatrixCasFunctions])

  /*test("perf")*/ {

    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (value <- row) yield {
          calculate(value, comparator, threshold, operand)
        }
      }
    }

    LOGGER.info("test started")
    val warmMatrix = new Matrix[Float](1, 1)
    Memory.print("GPU memory usage")
    val data = NativeMatrix.matrixData[Float](100000, 5000)
    LOGGER.info("data created")
    val matrix = Matrix.of(data)
    LOGGER.info("matrix created")
    Memory.print("GPU memory usage")
    val threshold = 11.3f
    val operand = 100.4f

    //warm up
    for (i <- 0 to 10) {
      Matrix.Cas.cas(matrix, Comparator.<=, threshold, operand)
    }
    Memory.print("GPU memory usage")

    val result = Matrix.Cas.cas(matrix, Comparator.<=, threshold, operand)
    val nativeResult = cas(data, Comparator.<=, threshold, operand)
    val maxError = compare(result.values(), nativeResult)
//    assert(maxError === 0, s"${Comparator.<=}")
    assert(maxError == 0, s"${Comparator.<=}")

    val stats = new Statistics("perf")
    for (attempt <- 0 to 100) {
      val t1 = System.nanoTime()
      for (i <- 0 to 10) {
        Matrix.Cas.cas(matrix, Comparator.<=, threshold, operand)
      }
      val t2 = System.nanoTime()

      LOGGER.info(s"performance: ${(t2 - t1) / 10}")
      stats.addValue((t2 - t1) / 10)
    }
    LOGGER.info(s"average: ${stats.getAverage}")
  }

}

//(2, 2, 2)     348602072
//(2, 2, 16)
//(2, 2, 32)     34070692
//(2, 2, 64)
//(2, 2, 128)
//(2, 2, 256)
//(2, 2, 512)    39632975

//(4, 2, 512)    32317927

//(8, 2, 2)     222176472
//(8, 2, 16)     38562604
//(8, 2, 32)     29538657
//(8, 2, 64)     29530330 +++++
//(8, 2, 128)    29606379
//(8, 2, 256)    29741517
//(8, 2, 512)    30095744

//(8, 4, 64)     30738239

//(16, 2, 2)    206788338
//(16, 2, 16)    38376714
//(16, 2, 32)    30529872
//(16, 2, 64)    30502301
//(16, 2, 128)   30489526
//(16, 2, 256)   30494368
//(16, 2, 512)   30167496

//(16, 4, 2)    122394181
//(16, 4, 16)    31299378
//(16, 4, 32)    30762136
//(16, 4, 64)    30813794
//(16, 4, 128)   30993021
//(16, 4, 256)   31425457

//(16, 8, 128)   34346779

//(32, 2, 2)    202540770
//(32, 2, 16)
//(32, 2, 32)    31099615
//(32, 2, 64)    31109574
//(32, 2, 128)
//(32, 2, 256)
//(32, 2, 512)   30277664

//(32, 4, 2)    116983402
//(32, 4, 16)
//(32, 4, 32)    33268903
//(32, 4, 64)
//(32, 4, 128)
//(32, 4, 256)   32990042

//(64, 4, 32)    34791692
//(128, 32, 32)  32193152
//(256, 32, 32)  32006590
//(512, 32, 32)  32008500
//(1024, 32, 32) 32099730