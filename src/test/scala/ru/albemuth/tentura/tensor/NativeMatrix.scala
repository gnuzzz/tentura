package ru.albemuth.tentura.tensor

import NativeMatrix.emptyMatrix
import NativeVector.emptyVector

import java.util.Random

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class NativeMatrix(v: Array[Array[Float]]) {

  val data: Array[Array[Float]] = v
  val rows: Int = data.length
  val columns: Int = if (rows > 0) data(0).length else 0

  def apply(columnsIndices: NativeVector): NativeVector = {
    val result = emptyVector(rows)

    for (i <- data.indices) {
      val row = data(i)
      result.data(i) = row(columnsIndices.data(i).toInt)
    }

    result
  }

  def +(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val matrixRow = matrix.data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) + matrixRow(j)
      }
    }

    result
  }

  def +(vector: NativeVector): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      for (j <- data(i).indices) {
        result.data(i)(j) = data(i)(j) + vector.data(j)
      }
    }

    result
  }

  def +|(vector: NativeVector): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      for (j <- data(i).indices) {
        result.data(i)(j) = data(i)(j) + vector.data(i)
      }
    }

    result
  }

  def +(scalar: Float): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) + scalar
      }
    }

    result
  }

  def -(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val matrixRow = matrix.data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) - matrixRow(j)
      }
    }

    result
  }

  def -(vector: NativeVector): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      for (j <- data(i).indices) {
        result.data(i)(j) = data(i)(j) - vector.data(j)
      }
    }

    result
  }

  def -|(vector: NativeVector): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      for (j <- data(i).indices) {
        result.data(i)(j) = data(i)(j) - vector.data(i)
      }
    }

    result
  }

  def -(scalar: Float): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) - scalar
      }
    }

    result
  }

  def *(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(rows, matrix.columns)

    for (i <- result.data.indices) {
      val resultRow = result.data(i)
      for (j <- resultRow.indices) {
        var cij = 0.0f
        for (k <- data(i).indices) {
          cij += data(i)(k) * matrix.data(k)(j)
        }
        resultRow(j) = cij
      }
    }

    result
  }

  def *(vector: NativeVector): NativeVector = {
    val result = emptyVector(rows)

    for (i <- result.data.indices) {
      var ri = 0.0f
      for (j <- data(i).indices) {
        ri += data(i)(j) * vector.data(j)
      }
      result.data(i) = ri
    }

    result
  }

  def *(scalar: Float): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) * scalar
      }
    }

    result
  }

  def :*(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val matrixRow = matrix.data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) * matrixRow(j)
      }
    }

    result
  }

  def /(scalar: Float): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) / scalar
      }
    }

    result
  }

  def :/(matrix: NativeMatrix): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val matrixRow = matrix.data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) / matrixRow(j)
      }
    }

    result
  }

  def pow(power: Float): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = Math.pow(row(j), power).toFloat
      }
    }

    result
  }

  def pow2(): NativeMatrix = {
    val result = emptyMatrix(rows, columns)

    for (i <- data.indices) {
      val row = data(i)
      val resultRow = result.data(i)
      for (j <- row.indices) {
        resultRow(j) = row(j) * row(j)
      }
    }

    result
  }

  def ^(power: Float): NativeMatrix = {
    if (power == 2) {
      pow2()
    } else {
      pow(power)
    }
  }

  def t: NativeMatrix = {
    val result = emptyMatrix(columns, rows)

    for (i <- data.indices) {
      val row = data(i)
      for (j <- row.indices) {
        result.data(j)(i) = row(j)
      }
    }

    result
  }

  def row(i: Int): NativeVector = {
    NativeVector(data(i))
  }

  def column(j: Int): NativeVector = {
    val result = emptyVector(rows)

    for (i <- data.indices) {
      val row = data(i)
      result.data(i) = row(j)
    }

    result
  }

  def sum(): Float = {
    (for (row <- data) yield row.sum).sum
  }

  def sum(axis: Int): NativeVector = {
    if (axis == 0) {
      val resultData = new Array[Float](columns)
      for (row <- data) {
        for (i <- row.indices) {
          resultData(i) += row(i)
        }
      }
      NativeVector(resultData)
    } else {
      NativeVector(for (row <- data) yield row.sum)
    }
  }

}

object NativeMatrix {

  def apply(v: Array[Array[Float]]) = new NativeMatrix(v)

  def matrix(rows: Int, columns: Int): NativeMatrix = {
    val data = emptyMatrixData[Float](rows, columns)
    fillMatrixData[Float](data)
    NativeMatrix(data)
  }

  def emptyMatrix(rows: Int, columns: Int): NativeMatrix = {
    NativeMatrix(emptyMatrixData(rows, columns))
  }

  def matrixData[T: ClassTag](rows: Int, columns: Int): Array[Array[T]] = {
    val data = emptyMatrixData[T](rows, columns)
    fillMatrixData[T](data)
    data
  }

  def matrixData[T: ClassTag](rows: Int, columns: Int, generator: => T): Array[Array[T]] = {
    val data = emptyMatrixData[T](rows, columns)
    fillMatrixData[T](data, generator)
    data
  }

  def emptyMatrixData[T: ClassTag](rows: Int, columns: Int): Array[Array[T]] = {
    val data = new Array[Array[T]](rows)
    for (i <- data.indices) {
      data(i) = new Array[T](columns)
    }
    data
  }

  def fillMatrixData[T: ClassTag](data: Array[Array[T]]): Unit = {
    val rnd = new Random
    for (i <- data.indices) {
      val row = data(i)
      for (j <- row.indices) {
        row(j) = nextRandom[T](rnd.nextGaussian())
      }
    }
  }

  def fillMatrixData[T: ClassTag](data: Array[Array[T]], generator: => T): Unit = {
    for (i <- data.indices) {
      val row = data(i)
      for (j <- row.indices) {
        row(j) = generator
      }
    }
  }

  private def nextRandom[T: ClassTag](value: Double): T = {
    val tag = implicitly[ClassTag[T]]
    tag.runtimeClass match {
      case java.lang.Byte.TYPE      => (value * 100).toByte.asInstanceOf[T]
      case java.lang.Short.TYPE     => (value * 100).toShort.asInstanceOf[T]
      case java.lang.Character.TYPE => (value * 100).toChar.asInstanceOf[T]
      case java.lang.Integer.TYPE   => (value * 100).toInt.asInstanceOf[T]
      case java.lang.Long.TYPE      => (value * 100).toLong.asInstanceOf[T]
      case java.lang.Float.TYPE     => (value * 100).toFloat.asInstanceOf[T]
      case java.lang.Double.TYPE    => value.asInstanceOf[T]
      case java.lang.Boolean.TYPE   => (value > 0).asInstanceOf[T]
      case java.lang.Void.TYPE      => throw new IllegalArgumentException
      case _                        => throw new IllegalArgumentException
    }
  }

  implicit class ScalarFloat(scalar: Float) {

    def -(matrix: NativeMatrix): NativeMatrix = {
      val result = emptyMatrix(matrix.rows, matrix.columns)

      for (i <- matrix.data.indices; j <- matrix.data(i).indices) {
        result.data(i)(j) = scalar - matrix.data(i)(j)
      }

      result
    }

    def /(matrix: NativeMatrix): NativeMatrix = {
      val result = emptyMatrix(matrix.rows, matrix.columns)

      for (i <- matrix.data.indices; j <- matrix.data(i).indices) {
        result.data(i)(j) = scalar / matrix.data(i)(j)
      }

      result
    }
  }

}