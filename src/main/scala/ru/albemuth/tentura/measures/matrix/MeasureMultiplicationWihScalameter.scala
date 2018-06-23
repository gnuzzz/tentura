package ru.albemuth.tentura.measures.matrix

import org.scalameter.{Key, Warmer, config}
import ru.albemuth.tentura.tensor.Matrix

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object MeasureMultiplicationWihScalameter extends App with MatrixUtils {

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  val a = Matrix.of(createMatrix())
  val b = Matrix.of(createMatrix())
  implicit val c = new Matrix[Float](a.rows, b.columns)

  val multiplyTime = standardConfig measure {
    val result = a *** b
  }

  println(s"Time: $multiplyTime")

}
