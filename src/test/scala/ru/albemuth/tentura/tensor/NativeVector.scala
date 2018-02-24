package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.tensor.NativeVector.emptyVector
import ru.albemuth.tentura.tensor.NativeMatrix.emptyMatrix

import java.util.Random

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class NativeVector(v: Array[Float]) {

  val data: Array[Float] = v
  def length: Int = data.length

  def +(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(matrix.rows, matrix.columns)

    for (i <- matrix.data.indices) {
      for (j <- matrix.data(i).indices) {
        result.data(i)(j) = data(j) + matrix.data(i)(j)
      }
    }

    result
  }

  def +|(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(matrix.rows, matrix.columns)

    for (i <- matrix.data.indices) {
      for (j <- matrix.data(i).indices) {
        result.data(i)(j) = data(i) + matrix.data(i)(j)
      }
    }

    result
  }

  def +(vector: NativeVector): NativeVector = {
    val result = emptyVector(length)

    for (i <- result.data.indices) {
      result.data(i) = data(i) + vector.data(i)
    }

    result
  }

  def +(scalar: Float): NativeVector = {
    val result = emptyVector(length)

    for (i <- result.data.indices) {
      result.data(i) = data(i) + scalar
    }

    result
  }

  def -(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(matrix.rows, matrix.columns)

    for (i <- matrix.data.indices) {
      for (j <- matrix.data(i).indices) {
        result.data(i)(j) = data(j) - matrix.data(i)(j)
      }
    }

    result
  }

  def -|(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(matrix.rows, matrix.columns)

    for (i <- matrix.data.indices) {
      for (j <- matrix.data(i).indices) {
        result.data(i)(j) = data(i) - matrix.data(i)(j)
      }
    }

    result
  }

  def -(vector: NativeVector): NativeVector = {
    val result = emptyVector(length)

    for (i <- result.data.indices) {
      result.data(i) = data(i) - vector.data(i)
    }

    result
  }

  def -(scalar: Float): NativeVector = {
    val result = emptyVector(length)

    for (i <- result.data.indices) {
      result.data(i) = data(i) - scalar
    }

    result
  }

  def *(scalar: Float): NativeVector = {
    val result = emptyVector(length)

    for (i <- result.data.indices) {
      result.data(i) = data(i) * scalar
    }

    result
  }

  def *(vector: NativeVector): Float = {
    var result = 0.0f
    for (i <- data.indices) {
      result += data(i) * vector.data(i)
    }
    result
  }

  def *(matrix: NativeMatrix): NativeVector = {
    val result = emptyVector(matrix.columns)

    for (i <- result.data.indices) {
      var ri = 0.0f
      for (j <- matrix.data.indices) {
        ri += data(j) * matrix.data(j)(i)
      }
      result.data(i) = ri
    }

    result
  }

  def **(vector: NativeVector): NativeMatrix = {
    val result = emptyMatrix(length, vector.length)

    for (i <- data.indices; j <- vector.data.indices) {
      result.data(i)(j) = data(i) * vector.data(j)
    }

    result
  }

  def :*(vector: NativeVector): NativeVector = {
    val result = emptyVector(length)

    for (i <- data.indices) {
      result.data(i) = data(i) * vector.data(i)
    }

    result
  }

  def /(scalar: Float): NativeVector = {
    val result = emptyVector(length)

    for (i <- data.indices) {
      result.data(i) = data(i) / scalar
    }

    result
  }

  def :/(vector: NativeVector): NativeVector = {
    val result = emptyVector(length)

    for (i <- data.indices) {
      result.data(i) = data(i) / vector.data(i)
    }

    result
  }

  def sigmoid(): NativeVector = {
    val result = emptyVector(length)
    for (i <- data.indices) {
      result.data(i) = (1.0 / (1.0 + Math.exp(-data(i)))).toFloat
    }
    result
  }

  def pow2(): NativeVector = {
    val result = emptyVector(length)
    for (i <- data.indices) {
      result.data(i) = data(i) * data(i)
    }
    result
  }

  def pow(power: Float): NativeVector = {
    val result = emptyVector(length)
    for (i <- data.indices) {
      result.data(i) = Math.pow(data(i), power).toFloat
    }
    result
  }

  def ^(power: Float): NativeVector = {
    if (power == 2) {
      pow2()
    } else {
      pow(power)
    }
  }

  def exp(): NativeVector = {
    val result = emptyVector(length)
    for (i <- data.indices) {
      result.data(i) = Math.exp(data(i)).toFloat
    }
    result
  }

  def sum(): Float = {
    data.sum
  }

}

object NativeVector {

  def apply(v: Array[Float]) = new NativeVector(v)

  def vector(length: Int): NativeVector = {
    val data = emptyVectorData(length)
    fillVectorData(data)
    NativeVector(data)
  }

  def emptyVector(length: Int): NativeVector = {
    NativeVector(emptyVectorData(length))
  }

  def vectorData(length: Int): Array[Float] = {
    val data = emptyVectorData(length)
    fillVectorData(data)
    data
  }

  def emptyVectorData(length: Int): Array[Float] = {
    new Array[Float](length)
  }

  def fillVectorData(data: Array[Float]): Unit = {
    val rnd = new Random
    for (i <- data.indices) {
      data(i) = rnd.nextGaussian.toFloat
    }
  }

  implicit class ScalarFloat(scalar: Float) {

    def -(vector: NativeVector): NativeVector = {
      val result = emptyVector(vector.length)

      for (i <- vector.data.indices) {
        result.data(i) = scalar - vector.data(i)
      }

      result
    }

    def /(vector: NativeVector): NativeVector = {
      val result = emptyVector(vector.length)

      for (i <- vector.data.indices) {
        result.data(i) = scalar / vector.data(i)
      }

      result
    }
  }

}