package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait TestUtils {

  val ROWS = 513
  val COLUMNS = 131

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

  def errors(data1: Array[Array[Float]], data2: Array[Array[Float]]): Seq[(Int, Int, Double)] = {
    for (i <- data1.indices; j <- data1(i).indices) yield {
      val error = if (data1(i)(j) == 0 || data2(i)(j) == 0){
        Math.abs(data1(i)(j) - data2(i)(j))
      } else {
        Math.abs(data1(i)(j) - data2(i)(j)) / Math.abs(data2(i)(j)).toDouble
      }
      (i, j, error)
    }
  }

  def compare(data1: Array[Array[Float]], data2: Array[Array[Float]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == 0 || row2(j) == 0){
          Math.abs(row1(j) - row2(j))
        } else {
          Math.abs(row1(j) - row2(j)) / Math.abs(row2(j)).toDouble
        }
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Array[Double]], data2: Array[Array[Double]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == 0 || row2(j) == 0) {
          Math.abs(row1(j) - row2(j))
        } else {
          Math.abs(row1(j) - row2(j)) / Math.abs(row2(j))
        }
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Array[Int]], data2: Array[Array[Int]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == 0 || row2(j) == 0){
          Math.abs(row1(j) - row2(j))
        } else {
          Math.abs(row1(j) - row2(j)) / Math.abs(row2(j)).toDouble
        }
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Array[Long]], data2: Array[Array[Long]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == 0 || row2(j) == 0){
          Math.abs(row1(j) - row2(j))
        } else {
          Math.abs(row1(j) - row2(j)) / Math.abs(row2(j)).toDouble
        }
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Array[Boolean]], data2: Array[Array[Boolean]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == row2(j)) 0 else 1
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Array[Char]], data2: Array[Array[Char]]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = if (row1(j) == row2(j)) 0 else 1
        if (error > maxError) maxError = error
      }
    }
    maxError
  }

  def compare(data1: Array[Int], data2: Array[Int]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        Math.abs(data1(i) - data2(i))
      } else {
        Math.abs(data1(i) - data2(i)) / Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Long], data2: Array[Long]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        Math.abs(data1(i) - data2(i))
      } else {
        Math.abs(data1(i) - data2(i)) / Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Boolean], data2: Array[Boolean]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == data2(i)) 0 else 1
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Char], data2: Array[Char]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == data2(i)) 0 else 1
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Float], data2: Array[Float]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        Math.abs(data1(i) - data2(i))
      } else {
        Math.abs(data1(i) - data2(i)) / Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Double], data2: Array[Double]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        Math.abs(data1(i) - data2(i))
      } else {
        Math.abs(data1(i) - data2(i)) / Math.abs(data2(i))
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

}
