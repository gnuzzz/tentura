package ru.albemuth.tentura.tensor

import Matrix._
import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, pointer}
import ru.albemuth.tentura.tensor.kernel._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Matrix[T: ClassTag](val rows: Int, val columns: Int, override protected[tensor] val data: Array[T]) extends DeviceVar[T] {

  override protected[tensor] val dataPointer: Pointer = pointer(data)
  override protected[tensor] val deviceDataPtr: CUdeviceptr = devicePtr(data)

  copy2device()

  def this(r: Int, c: Int) {
    this(r, c, new Array[T](r * c))
  }

  def apply(i: Int): Vector[T] = row(i)

  def apply(i: Int, j: Int): T = data(i * columns + j)

  def apply(columnsIndices: Vector[Int]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixColumnsValues, columns, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(columnsIndices.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixColumnsValues.launch(params, result)

    result
  }

  def values(): Array[Array[T]] = {
    Matrix.values(rows, columns)(data)
  }

  protected[tensor] def result(kernel: MatrixKernel, resultKey: Any, result: => Matrix[T]): Matrix[T] = {
    resultsCache.result[T, Matrix[T]](kernel, resultKey, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixAddMatrix, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixAddMatrix.launch(params, result)

    result
  }

  def +(vector: Vector[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixAddRow, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    matrixAddRow.launch(params, result)

    result
  }

  def +|(vector: Vector[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixAddColumn, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    matrixAddColumn.launch(params, result)

    result
  }

  def +(scalar: Float): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixAddScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixAddScalar.launch(params, result)

    result
  }

  def +(scalar: Double): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixAddScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixAddScalar.launch(params, result)

    result
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixSubMatrix, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixSubMatrix.launch(params, result)

    result
  }

  def -(vector: Vector[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixSubRow, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    matrixSubRow.launch(params, result)

    result
  }

  def -|(vector: Vector[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixSubColumn, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    matrixSubColumn.launch(params, result)

    result
  }

  def -(scalar: Float): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixSubScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixSubScalar.launch(params, result)

    result
  }

  def *(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixMulMatrix, matrix, new Matrix[T](rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns)),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](result.rows)), Pointer.to(Array[Int](result.columns))
    )

    matrixMulMatrix.launch(params, result)

    result
  }

  def *(vector: Vector[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixMulVector, vector, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns)),
      Pointer.to(Array[Int](vector.length)), Pointer.to(Array[Int](result.length))
    )

    matrixMulVector.launch(params, result)

    result
  }

  def *(scalar: Float): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixMulScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixMulScalar.launch(params, result)

    result
  }

  def *(scalar: Double): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixMulScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixMulScalar.launch(params, result)

    result
  }

  def :*(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixElementWiseMulMatrix, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixElementWiseMulMatrix.launch(params, result)

    result
  }

  def /(scalar: Float): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixDivScalar, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixDivScalar.launch(params, result)

    result
  }

  def :/(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixElementWiseDivMatrix, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixElementWiseDivMatrix.launch(params, result)

    result
  }

  def pow(power: Float): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixPow, power, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](power)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixPow.launch(params, result)

    result
  }

  def pow2(): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixPow, 2, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixPow2.launch(params, result)

    result
  }

  def ^(power: Float): Matrix[T] = {
    if (power == 2) {
      pow2()
    } else {
      pow(power)
    }
  }

  def t: Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](matrixTranspose, Unit, new Matrix[T](columns, rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixTranspose.launch(params, result)

    result
  }

  def row(i: Int): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixRow, Unit, new Vector[T](columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](i)),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixRow.launch(params, result)

    result
  }

  def column(j: Int): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixColumn, Unit, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](j)),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixColumn.launch(params, result)

    result
  }

  def sum(): Scalar[T] = {
    val result = resultsCache.result[T, Scalar[T]](matrixSum, this, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    matrixSum.launch(params, result)

    result
  }

  def sum(axis: Int): Vector[T] = {
    if (axis == 0) {
      sumRows()
    } else {
      sumColumns()
    }
  }

  private def sumRows(): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixSumRows, Unit, new Vector[T](columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixSumRows.launch(params, result)

    result
  }

  private def sumColumns(): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](matrixSumColumns, Unit, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    matrixSumColumns.launch(params, result)

    result
  }
}

object Matrix {

  lazy val matrixAddMatrix = new MatrixAddMatrix
  lazy val matrixAddScalar = new MatrixAddScalar
  lazy val matrixColumn = new MatrixColumn
  lazy val matrixDivScalar = new MatrixDivScalar
  lazy val matrixElementWiseDivMatrix = new MatrixElementWiseDivMatrix
  lazy val matrixElementWiseMulMatrix = new MatrixElementWiseMulMatrix
  lazy val matrixMulMatrix = new MatrixMulMatrix
  lazy val matrixMulScalar = new MatrixMulScalar
  lazy val matrixMulVector = new MatrixMulVector
  lazy val matrixRow = new MatrixRow
  lazy val matrixSubMatrix = new MatrixSubMatrix
  lazy val matrixSubScalar = new MatrixSubScalar
  lazy val matrixTranspose = new MatrixTranspose
  lazy val matrixPow = new MatrixPow
  lazy val matrixPow2 = new MatrixPow2
  lazy val matrixSum = new MatrixSum
  lazy val matrixSumRows = new MatrixSumRows
  lazy val matrixSumColumns = new MatrixSumColumns
  lazy val matrixAddRow = new MatrixAddRow
  lazy val matrixAddColumn = new MatrixAddColumn
  lazy val matrixSubRow = new MatrixSubRow
  lazy val matrixSubColumn = new MatrixSubColumn
  lazy val matrixColumnsValues = new MatrixColumnsValues

  def apply[T: ClassTag](rows: Int, columns: Int): MatrixBuilder[T] = {
    new MatrixBuilder[T](rows, columns)
  }

  def of[T: ClassTag](values: Array[Array[T]]): Matrix[T] = {
    val rows = Matrix.rows(values)
    val columns = Matrix.columns(values)
    new Matrix(rows, columns, data(rows, columns)(values))
  }

  def data[T: ClassTag](rows: Int, columns: Int)(values: Array[Array[T]]): Array[T] = {
    val data: Array[T] = Array.ofDim(rows * columns)
    for (rowIndex <- values.indices) {
      System.arraycopy(values(rowIndex), 0, data, rowIndex * columns, columns)
    }
    data
  }

  def values[T: ClassTag](rows: Int, columns: Int)(data: Array[T]): Array[Array[T]] = {
    val ret = new Array[Array[T]](rows)
    for (rowIndex <- 0 until rows) {
      ret(rowIndex) = new Array[T](columns)
      System.arraycopy(data, rowIndex * columns, ret(rowIndex), 0, columns)
    }
    ret
  }

  def rows[T: ClassTag](values: Array[Array[T]]): Int = {
    values.length
  }

  def columns[T: ClassTag](matrix: Array[Array[T]]): Int = {
    if (matrix.length > 0) matrix(0).length else 0
  }

  def newValues[T: ClassTag](rows: Int, columns: Int): Array[Array[T]] = {
    val ret = new Array[Array[T]](rows)
    for (rowIndex <- 0 until rows) {
      ret(rowIndex) = new Array[T](columns)
    }
    ret
  }

  class MatrixBuilder[T: ClassTag](rows: Int, columns: Int) {

    def of(value: => T): Matrix[T] = {
      new Matrix(rows, columns, Array.fill(rows * columns)(value))
    }

    def of(values: Array[T]): Matrix[T] = {
      new Matrix(rows, columns, values)
    }

    def of(values: Array[Array[T]]): Matrix[T] = {
      new Matrix(values.length, values(0).length, data(rows, columns)(values))
    }

    def diag(value: => T): Matrix[T] = {
      val data: Array[T] = Array.ofDim(rows * columns)
      for (i <- data.indices) {
        data(i * (columns + 1)) = value
      }
      new Matrix(rows, columns, data)
    }
  }
}
