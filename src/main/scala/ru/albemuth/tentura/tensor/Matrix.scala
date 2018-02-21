package ru.albemuth.tentura.tensor

import Matrix._
import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, pointer}
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelTemplate}
import ru.albemuth.tentura.tensor.MathFunctions.{pow, pow2, powd, pow2d}
import ru.albemuth.tentura.tensor.kernel.matrix._

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
    val kernel = matrixColumnsValues.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, columns, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(columnsIndices.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def values(): Array[Array[T]] = {
    Matrix.values(rows, columns)(data)
  }

  protected[tensor] def result[R: ClassTag](kernel: JCudaKernel, resultKey: Any, result: => Matrix[R]): Matrix[R] = {
    resultsCache.result[R, Matrix[R]](kernel, resultKey, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    val kernel = matrixAddMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(vector: Vector[T]): Matrix[T] = {
    val kernel = matrixAddRow.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    kernel.launch(params, result)

    result
  }

  def +|(vector: Vector[T]): Matrix[T] = {
    val kernel = matrixAddColumn.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Byte): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(scalar: Short): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(scalar: Int): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(scalar: Long): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(scalar: Float): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def +(scalar: Double): Matrix[T] = {
    val kernel = matrixAddScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    val kernel = matrixSubMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(vector: Vector[T]): Matrix[T] = {
    val kernel = matrixSubRow.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    kernel.launch(params, result)

    result
  }

  def -|(vector: Vector[T]): Matrix[T] = {
    val kernel = matrixSubColumn.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, vector, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Byte): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(scalar: Short): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(scalar: Int): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(scalar: Long): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(scalar: Float): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def -(scalar: Double): Matrix[T] = {
    val kernel = matrixSubScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(matrix: Matrix[T]): Matrix[T] = {
    val kernel = matrixMulMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns)),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](result.rows)), Pointer.to(Array[Int](result.columns))
    )

    kernel.launch(params, result)

    result
  }

  def *(vector: Vector[T]): Vector[T] = {
    val kernel = matrixMulVector.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, vector, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns)),
      Pointer.to(Array[Int](vector.length)), Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Byte): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(scalar: Short): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(scalar: Int): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(scalar: Long): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(scalar: Float): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def *(scalar: Double): Matrix[T] = {
    val kernel = matrixMulScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def :*(matrix: Matrix[T]): Matrix[T] = {
    val kernel = matrixElementWiseMulMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Byte): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Short): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Int): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Long): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Float): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def /(scalar: Double): Matrix[T] = {
    val kernel = matrixDivScalar.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, scalar, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def :/(matrix: Matrix[T]): Matrix[T] = {
    val kernel = matrixElementWiseDivMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](rows, columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
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

  def t: Matrix[T] = {
    val kernel = matrixTranspose.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, Unit, new Matrix[T](columns, rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def row(i: Int): Vector[T] = {
    val kernel = matrixRow.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, Unit, new Vector[T](columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](i)),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def column(j: Int): Vector[T] = {
    val kernel = matrixColumn.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, Unit, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](j)),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  def sum(): Scalar[T] = {
    val kernel = matrixSum.kernel[T]
    val result = resultsCache.result[T, Scalar[T]](kernel, this, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )

    kernel.launch(params, result)

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
    val kernel = matrixSumRows.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, Unit, new Vector[T](columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
  }

  private def sumColumns(): Vector[T] = {
    val kernel = matrixSumColumns.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, Unit, new Vector[T](rows))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
    )
    kernel.launch(params, result)

    result
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
  lazy val matrixSum = new KernelTemplate(new MatrixSum)
  lazy val matrixSumRows = new KernelTemplate(new MatrixSumRows)
  lazy val matrixSumColumns = new KernelTemplate(new MatrixSumColumns)
  lazy val matrixAddRow = new KernelTemplate(new MatrixAddRow)
  lazy val matrixAddColumn = new KernelTemplate(new MatrixAddColumn)
  lazy val matrixSubRow = new KernelTemplate(new MatrixSubRow)
  lazy val matrixSubColumn = new KernelTemplate(new MatrixSubColumn)
  lazy val matrixColumnsValues = new KernelTemplate(new MatrixColumnsValues)

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
