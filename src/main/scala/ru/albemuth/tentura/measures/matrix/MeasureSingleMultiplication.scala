package ru.albemuth.tentura.measures.matrix

import ru.albemuth.tentura.tensor.Matrix

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object MeasureSingleMultiplication extends App with MatrixUtils {

  val a = Matrix.of(createMatrix())
  val b = Matrix.of(createMatrix())
  implicit val c = new Matrix[Float](a.rows, b.columns)
  val warmResult = a * b

  val t1 = System.nanoTime()
  val result = a * b
  val t2 = System.nanoTime()

  result.copy2host()

  println(s"Time: ${t2 - t1}")

}
