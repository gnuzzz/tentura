package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait TestUtils {

  def printMatrix(matrix: Array[Array[Float]]): Unit = {
    for (i <- matrix.indices) {
      val row = matrix(i)
      for (j <- row.indices) {
        print(row(j) + " ")
      }
      println()
    }
  }

  def printVector(vector: Array[Float]): Unit = {
    for (i <- vector.indices) {
      print(vector(i) + " ")
    }
    println()
  }

  def errors(data1: Array[Array[Float]], data2: Array[Array[Float]]): Seq[(Int, Int, Float)] = {
    for (i <- data1.indices; j <- data1(i).indices) yield {
      (i, j, Math.abs(data1(i)(j) - data2(i)(j)))
    }
  }

  def compare(data1: Array[Array[Float]], data2: Array[Array[Float]]): Float = {
    var maxError = Float.MinValue
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = Math.abs(row1(j) - row2(j))
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Float], data2: Array[Float]): Float = {
    var maxError = Float.MinValue
    for (i <- data1.indices) {
      val error = Math.abs(data1(i) - data2(i))
      if (error > maxError) maxError = error
    }
    maxError
  }

}
