package ru.albemuth.tentura.performance

import org.scalameter.{Key, Warmer, config}
import ru.albemuth.tentura.tensor.Matrix

import java.util.Random

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object SamplePerformance extends App {

  val ROWS = 512
  val COLUMNS = 128

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def createMatrix(): Array[Array[Float]] = {
    val rnd = new Random
    val matrix = new Array[Array[Float]](ROWS)
    for (i <- matrix.indices) {
      matrix(i) = new Array[Float](COLUMNS)
      for (j <- matrix(i).indices) {
        matrix(i)(j) = rnd.nextGaussian.toFloat
      }
    }
    matrix
  }

  val a = Matrix.of(createMatrix())

  implicit val c = new Matrix[Float](a.columns, a.rows)

  val multiplyTime = standardConfig measure {
    val result = a.T
  }

  println(s"Time: $multiplyTime")

}
