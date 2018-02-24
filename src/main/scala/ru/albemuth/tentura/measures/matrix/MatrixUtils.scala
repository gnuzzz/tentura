package ru.albemuth.tentura.measures.matrix

import java.util.Random

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait MatrixUtils {

  val ROWS = 256
  val COLUMNS = 256

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

}
