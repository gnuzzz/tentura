package ru.albemuth.tentura.tensor

import Matrix._
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, sizeOf}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.MathFunctions.{pow, pow2, pow2d, powd}
import ru.albemuth.tentura.tensor.kernel.matrix._
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Matrix[T: ClassTag](override val deviceDataPtr: CUdeviceptr, val rows: Int, val columns: Int) extends DeviceVar[T] {

  def this(r: Int, c: Int) {
    this(devicePtr(r * c), r, c)
  }

  def this(data: Array[T], r: Int, c: Int) {
    this(devicePtr(r * c), r, c)
    copy2device(data)
  }

  def apply(i: Int): Vector[T] = {
    result("apply", i, new Vector(deviceDataPtr.withByteOffset(i * columns * sizeOf()), columns))
  }

  def apply(i: Int, j: Int): Scalar[T] = {
    result("apply", (i, j), new Scalar(deviceDataPtr.withByteOffset((i * columns + j) * sizeOf())))
  }

  def apply(columnsIndices: Vector[Int]): Vector[T] = {
    VectorKernel.vector(getColumnsValues, this, columnsIndices, new Vector[T](rows))
  }

  def apply(columnsIndices: Vector[Int], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(getColumnsValues, this, columnsIndices, result)
  }

  def update(i: Int, row: Vector[T]): Unit = {
    copy(row, 0, i * columns, row.length)
  }

  def update(i: Int, j: Int, value: T): Unit = {
    copy2device(value, i * columns + j)
  }

  def update(columnsIndices: Vector[Int], values: Vector[T]): Unit = {
    VectorKernel.vector_r(setColumnsValues, this, columnsIndices, values)
  }

  def values(): Array[Array[T]] = {
    val data = Array.ofDim[T](rows * columns)
    copy2host(data)
    Matrix.values(rows, columns)(data)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixAddMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def +(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddMatrix, this, matrix, result)
  }

  def +(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixAddRow, this, vector, new Matrix[T](rows, columns))
  }

  def +(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddRow, this, vector, result)
  }

  def +|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixAddColumn, this, vector, new Matrix[T](rows, columns))
  }

  def +|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddColumn, this, vector, result)
  }

  def +(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def +(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def +(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def +(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def +(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def +(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixAddScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddScalar, this, scalar, result)
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixSubMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def -(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubMatrix, this, matrix, result)
  }

  def -(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixSubRow, this, vector, new Matrix[T](rows, columns))
  }

  def -(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubRow, this, vector, result)
  }

  def -|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixSubColumn, this, vector, new Matrix[T](rows, columns))
  }

  def -|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubColumn, this, vector, result)
  }

  def -(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def -(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def -(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def -(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def -(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def -(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixSubScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubScalar, this, scalar, result)
  }

  def *(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2(matrixMulMatrix, this, matrix, new Matrix[T](rows, matrix.columns))
  }

  def *(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2_r(matrixMulMatrix, this, matrix, result)
  }

  def *(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(matrixMulVector, this, vector, new Vector[T](rows))
  }

  def *(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixMulVector, this, vector, result)
  }

  def *(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def *(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def *(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def *(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def *(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def *(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixMulScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMulScalar, this, scalar, result)
  }

  def :*(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixElementWiseMulMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def :*(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixElementWiseMulMatrix, this, matrix, result)
  }

  def /(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def :/(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixElementWiseDivMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def :/(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixElementWiseDivMatrix, this, matrix, result)
  }

  def ^(power: Float): Matrix[Float] = {
    if (power == 2) {
      pow2(this)
    } else {
      pow(this, power)
    }
  }

  def ^(power: Double): Matrix[Double] = {
    if (power == 2) {
      pow2d(this)
    } else {
      powd(this, power)
    }
  }

  def T: Matrix[T] = {
    MatrixKernel.matrix(matrixTranspose, this, new Matrix[T](columns, rows))
  }

  def T(result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTranspose, this, result)
  }

  def row(i: Int): Vector[T] = {
    VectorKernel.vector(matrixRow, this, i, new Vector[T](columns))
  }

  def row(i: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixRow, this, i, result)
  }

  def column(j: Int): Vector[T] = {
    VectorKernel.vector(matrixColumn, this, j, new Vector[T](columns))
  }

  def column(j: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixColumn, this, j, result)
  }

  def sum(): Scalar[T] = {
    MatrixFunctions.sum(this)
  }

  def sum(axis: Int): Vector[T] = {
    MatrixFunctions.sum(this, axis)
  }

}

object Matrix {

  lazy val matrixAddMatrix = new KernelTemplate(new MatrixAddMatrix)
  lazy val matrixAddScalar = new KernelTemplate(new MatrixAddScalar)
  lazy val matrixColumn = new KernelTemplate(new MatrixColumn)
  lazy val matrixDivScalar = new KernelTemplate(new MatrixDivScalar)
  lazy val matrixElementWiseDivMatrix = new KernelTemplate(new MatrixElementWiseDivMatrix)
  lazy val matrixElementWiseMulMatrix = new KernelTemplate(new MatrixElementWiseMulMatrix)
  lazy val matrixMulMatrix = new KernelTemplate(new MatrixMulMatrix)
  lazy val matrixMulScalar = new KernelTemplate(new MatrixMulScalar)
  lazy val matrixMulVector = new KernelTemplate(new MatrixMulVector)
  lazy val matrixRow = new KernelTemplate(new MatrixRow)
  lazy val matrixSubMatrix = new KernelTemplate(new MatrixSubMatrix)
  lazy val matrixSubScalar = new KernelTemplate(new MatrixSubScalar)
  lazy val matrixTranspose = new KernelTemplate(new MatrixTranspose)
  lazy val matrixAddRow = new KernelTemplate(new MatrixAddRow)
  lazy val matrixAddColumn = new KernelTemplate(new MatrixAddColumn)
  lazy val matrixSubRow = new KernelTemplate(new MatrixSubRow)
  lazy val matrixSubColumn = new KernelTemplate(new MatrixSubColumn)
  lazy val getColumnsValues = new KernelTemplate(new GetColumnsValues)
  lazy val setColumnsValues = new KernelTemplate(new SetColumnsValues)

  def apply[T: ClassTag](rows: Int, columns: Int): MatrixBuilder[T] = {
    new MatrixBuilder[T](rows, columns)
  }

  def of[T: ClassTag](values: Array[Array[T]]): Matrix[T] = {
    val rows = Matrix.rows(values)
    val columns = Matrix.columns(values)
    new Matrix(data(rows, columns)(values), rows, columns)
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
      new Matrix(Array.fill(rows * columns)(value), rows, columns)
    }

    def of(values: Array[T]): Matrix[T] = {
      new Matrix(values, rows, columns)
    }

    def of(values: Array[Array[T]]): Matrix[T] = {
      new Matrix(data(rows, columns)(values), values.length, values(0).length)
    }

    def diag(value: => T): Matrix[T] = {
      val data: Array[T] = Array.ofDim(rows * columns)
      for (i <- data.indices) {
        data(i * (columns + 1)) = value
      }
      new Matrix(data, rows, columns)
    }
  }
}
