package ru.albemuth.tentura.tensor

import scala.reflect.ClassTag
import scala.util.Random

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

  def points(rowsCount: Int, columnsCount: Int): (Array[Int], Array[Int]) = {
    val allPoints = Random.shuffle(for (i <- 0 until rowsCount; j <- 0 until columnsCount) yield (i, j))
    val length = Random.nextInt(allPoints.length)
    val selectedPoints = allPoints.slice(0, length)
    val rows = Array.ofDim[Int](length)
    val columns = Array.ofDim[Int](length)
    for (i <- selectedPoints.indices) {
      rows(i) = selectedPoints(i)._1
      columns(i) = selectedPoints(i)._2
    }
    (rows, columns)
  }

  def errors(data1: Array[Float], data2: Array[Float]): Seq[(Int, Double)] = {
    for (i <- data1.indices) yield {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i)).toDouble
      }
      (i, error)
    }
  }

  def errors(data1: Array[Array[Float]], data2: Array[Array[Float]]): Seq[(Int, Int, Double)] = {
    for (i <- data1.indices; j <- data1(i).indices) yield {
      val error = if (data1(i)(j) == 0 || data2(i)(j) == 0) {
        java.lang.Math.abs(data1(i)(j) - data2(i)(j))
      } else {
        java.lang.Math.abs(data1(i)(j) - data2(i)(j)) / java.lang.Math.abs(data2(i)(j)).toDouble
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
        val error = if (row1(j) == 0 || row2(j) == 0) {
          java.lang.Math.abs(row1(j) - row2(j))
        } else {
          java.lang.Math.abs(row1(j) - row2(j)) / java.lang.Math.abs(row2(j)).toDouble
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
          java.lang.Math.abs(row1(j) - row2(j))
        } else {
          java.lang.Math.abs(row1(j) - row2(j)) / java.lang.Math.abs(row2(j))
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
        val error = if (row1(j) == 0 || row2(j) == 0) {
          java.lang.Math.abs(row1(j) - row2(j))
        } else {
          java.lang.Math.abs(row1(j) - row2(j)) / java.lang.Math.abs(row2(j)).toDouble
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
        val error = if (row1(j) == 0 || row2(j) == 0) {
          java.lang.Math.abs(row1(j) - row2(j))
        } else {
          java.lang.Math.abs(row1(j) - row2(j)) / java.lang.Math.abs(row2(j)).toDouble
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

  def compare[T: ClassTag](data1: Array[T], data2: Array[T]): Double = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => compare(data1.asInstanceOf[Array[Boolean]], data2.asInstanceOf[Array[Boolean]])
      case b if b == classOf[Byte] => compare(data1.asInstanceOf[Array[Byte]], data2.asInstanceOf[Array[Byte]])
      case c if c == classOf[Char] => compare(data1.asInstanceOf[Array[Char]], data2.asInstanceOf[Array[Char]])
      case s if s == classOf[Short] => compare(data1.asInstanceOf[Array[Short]], data2.asInstanceOf[Array[Short]])
      case i if i == classOf[Int] => compare(data1.asInstanceOf[Array[Int]], data2.asInstanceOf[Array[Int]])
      case l if l == classOf[Long] => compare(data1.asInstanceOf[Array[Long]], data2.asInstanceOf[Array[Long]])
      case f if f == classOf[Float] => compare(data1.asInstanceOf[Array[Float]], data2.asInstanceOf[Array[Float]])
      case d if d == classOf[Double] => compare(data1.asInstanceOf[Array[Double]], data2.asInstanceOf[Array[Double]])
      case _ => ??? //not supported
    }
  }

  def compare(data1: Array[Boolean], data2: Array[Boolean]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == data2(i)) 0 else 1
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Byte], data2: Array[Byte]): Double = {
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

  def compare(data1: Array[Short], data2: Array[Short]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Int], data2: Array[Int]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Long], data2: Array[Long]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Float], data2: Array[Float]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i)).toDouble
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def compare(data1: Array[Double], data2: Array[Double]): Double = {
    var maxError = 0.0
    for (i <- data1.indices) {
      val error = if (data1(i) == 0 || data2(i) == 0) {
        java.lang.Math.abs(data1(i) - data2(i))
      } else {
        java.lang.Math.abs(data1(i) - data2(i)) / java.lang.Math.abs(data2(i))
      }
      if (error > maxError) maxError = error
    }
    maxError
  }

  def vectorData[T: ClassTag](length: Int): Array[T] = {
    val data = NativeVector.vectorData(length)
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => data.map(_ > 0).asInstanceOf[Array[T]]
      case b if b == classOf[Byte] => data.map(_.toByte).asInstanceOf[Array[T]]
      case c if c == classOf[Char] => data.map(_.toChar).asInstanceOf[Array[T]]
      case s if s == classOf[Short] => data.map(_.toShort).asInstanceOf[Array[T]]
      case i if i == classOf[Int] => data.map(_.toInt).asInstanceOf[Array[T]]
      case l if l == classOf[Long] => data.map(_.toLong).asInstanceOf[Array[T]]
      case f if f == classOf[Float] => data.asInstanceOf[Array[T]]
      case d if d == classOf[Double] => data.map(_.toDouble).asInstanceOf[Array[T]]
      case _ => ??? //not supported
    }
  }

  def sorted[T: ClassTag](data: Array[T]): Array[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => data.asInstanceOf[Array[Boolean]].sorted.asInstanceOf[Array[T]]
      case b if b == classOf[Byte] => data.asInstanceOf[Array[Byte]].sorted.asInstanceOf[Array[T]]
      case c if c == classOf[Char] => data.asInstanceOf[Array[Char]].sorted.asInstanceOf[Array[T]]
      case s if s == classOf[Short] => data.asInstanceOf[Array[Short]].sorted.asInstanceOf[Array[T]]
      case i if i == classOf[Int] => data.asInstanceOf[Array[Int]].sorted.asInstanceOf[Array[T]]
      case l if l == classOf[Long] => data.asInstanceOf[Array[Long]].sorted.asInstanceOf[Array[T]]
      case f if f == classOf[Float] => data.asInstanceOf[Array[Float]].sorted.asInstanceOf[Array[T]]
      case d if d == classOf[Double] => data.asInstanceOf[Array[Double]].sorted.asInstanceOf[Array[T]]
      case _ => ??? //not supported
    }
  }

}
