package ru.albemuth.tentura.performance

import org.scalameter.{Key, MeasureBuilder, Warmer, config}
import ru.albemuth.tentura.tensor.{Matrix, NativeMatrix, NativeVector, Vector}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait VectorPerformance {

  val ROWS = 5120
  val COLUMNS = 1280

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default
}

object VectorMulVector extends App with VectorPerformance {
  //val ROWS = 51200
  //base: vector * vector time: 3.4711663600000002 ms
  //tiled: vector * vector time: 1.7379620800000004 ms
  val a = new Vector[Float](NativeVector.vectorData(ROWS))
  val b = new Vector[Float](NativeVector.vectorData(ROWS))

  val multiplyTime = standardConfig measure {
    val result = a * b
  }

  println(s"vector * vector time: $multiplyTime")
}

object MatrixMulVector extends App with VectorPerformance {
  //val ROWS = 5120
  //val COLUMNS = 1280
  //base: matrix * vector time: 8.17624164 ms
  //tiled: matrix * vector time: 11.7008886 ms
  val a = Matrix.of[Float](NativeMatrix.matrixData(ROWS, COLUMNS))
  val b = new Vector[Float](NativeVector.vectorData(COLUMNS))

  val multiplyTime = standardConfig measure {
    val result = a * b
  }

  println(s"matrix * vector time: $multiplyTime")
}

object VectorMulMatrix extends App with VectorPerformance {
  //val ROWS = 5120
  //val COLUMNS = 1280
  //base: vector * matrix time: 7.919216640000001 ms
  //tiled: vector * matrix time: 14.5034108 ms
  val a = new Vector[Float](NativeVector.vectorData(ROWS))
  val b = Matrix.of[Float](NativeMatrix.matrixData(ROWS, COLUMNS))

  val multiplyTime = standardConfig measure {
    val result = a * b
  }

  println(s"vector * matrix time: $multiplyTime")
}

object VectorMatrixMulVector extends App with VectorPerformance {
  //val ROWS = 5120
  //val COLUMNS = 1280
  //base: vector ** vector time: 7.499809599999999 ms
  //tiled: vector ** vector time: 9.63164892 ms
  val a = new Vector[Float](NativeVector.vectorData(ROWS))
  val b = new Vector[Float](NativeVector.vectorData(ROWS))

  val multiplyTime = standardConfig measure {
    val result = a ** b
  }

  println(s"vector ** vector time: $multiplyTime")
}

