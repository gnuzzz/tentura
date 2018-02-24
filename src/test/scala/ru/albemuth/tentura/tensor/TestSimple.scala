package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestSimple extends FunSuite {

  val ROWS = 512
  val COLUMNS = 128

  test("test") {
    val a = Matrix.of[Float](NativeMatrix.matrixData(ROWS, COLUMNS))
    val b = new Vector[Float](NativeVector.vectorData(COLUMNS))

    for (i <- 0 until 10) {
      val result = a * b
      println(result)
    }
  }

}
