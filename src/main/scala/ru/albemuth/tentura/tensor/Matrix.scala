package ru.albemuth.tentura.tensor

import Matrix._
import jcuda.driver.{CUdeviceptr, JCudaDriver}
import ru.albemuth.jcuda.jcusegsort.{KeySortContext, KeyValueSortContext, Sorting}
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{datatype, devicePtr, sizeOf}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Math.{pow, pow2, pow2d, powd}
import ru.albemuth.tentura.tensor.Operator.Operator
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.kernel.matrix._
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel.{scalar, scalar_r}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Matrix[T: ClassTag](override val deviceDataPtr: CUdeviceptr, val rows: Int, val columns: Int) extends DeviceVar[T] {

  def this(r: Int, c: Int) {
    this(devicePtr(r.toLong * c), r, c)
  }

  def this(data: Array[T], r: Int, c: Int) {
    this(devicePtr(r.toLong * c), r, c)
    copy2device(data)
  }

  def apply(i: Int, axis: Axis): Vector[T] = {
    if (axis == 0) {
      row(i)
    } else {
      column(i)
    }
  }

  def apply(i: Int, axis: Axis, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      row(i, result)
    } else {
      column(i, result)
    }
  }

  def apply(i: Int, j: Int): Scalar[T] = {
    val s = result("apply", (i, j), new Scalar[T]())
    s.copyFrom(this, i * columns + j, 0, 1)
    s
  }

  def apply(i: Int, j: Int, result: Scalar[T]): Scalar[T] = {
    result.copyFrom(this, (i * columns + j) * sizeOf(), 0, sizeOf())
    result
  }

  def apply(indices: Vector[Int], axis: Axis): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector(getColumnsValues, this, indices, new Vector[T](rows))
    } else {
      VectorKernel.vector(getRowsValues, this, indices, new Vector[T](columns))
    }
  }

  def apply(indices: Vector[Int], axis: Axis, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector_r(getColumnsValues, this, indices, result)
    } else {
      VectorKernel.vector_r(getRowsValues, this, indices, result)
    }
  }

  def apply(rowsIndices: Vector[Int], columnsIndices: Vector[Int]): Vector[T] = {
    VectorKernel.vector(getIndexedValues, this, rowsIndices, columnsIndices, new Vector[T](rowsIndices.length))
  }

  def apply(rowsIndices: Vector[Int], columnsIndices: Vector[Int], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(getIndexedValues, this, rowsIndices, columnsIndices, result)
  }

  def update(i: Int, axis: Int, values: Vector[T]): Unit = {
    if (axis == 0) {
      copyFrom(values, 0, i * columns, values.length)
    } else {
      VectorKernel.vector_r(updateColumn, this, i, values)
    }
  }

  def update(i: Int, j: Int, value: T): Unit = {
    copy2device(value, i * columns + j)
  }

  def update(rowsIndices: Vector[Int], columnsIndices: Vector[Int], value: T): Unit = {
    VectorKernel.unit(setIndexedValue, this, rowsIndices, columnsIndices, value)
  }

  def update(rowsIndices: Vector[Int], columnsIndices: Vector[Int], values: Vector[T]): Unit = {
    VectorKernel.vector_r(setIndexedValues, this, rowsIndices, columnsIndices, values)
  }

  def update(indices: Vector[Int], axis: Axis, value: T): Unit = {
    if (axis == 0) {
      VectorKernel.unit(setColumnsValue, this, indices, value)
    } else {
      VectorKernel.unit(setRowsValue, this, indices, value)
    }
  }

  def update(indices: Vector[Int], axis: Axis, values: Vector[T]): Unit = {
    if (axis == 0) {
      VectorKernel.vector_r(setColumnsValues, this, indices, values)
    } else {
      VectorKernel.vector_r(setRowsValues, this, indices, values)
    }
  }

  def slice(from: Int, to: Int, axis: Axis): Matrix[T] = {
    if (axis == 0) {
      val result = new Matrix[T](to - from, columns)
      val itemSize = sizeOf()
      JCudaDriver.cuMemcpyDtoD(result.deviceDataPtr, deviceDataPtr.withByteOffset(from * columns * itemSize), (to - from) * columns * itemSize)
      result
    } else {
      //todo - use JCudaDriver.cuMemcpy2D
      MatrixKernel.matrix(sliceColumns, this, from, to, new Matrix[T](rows, to - from))
    }
  }

  def slice(from: Int, to: Int, axis: Axis, result: Matrix[T]): Matrix[T] = {
    if (axis == 0) {
      val itemSize = sizeOf()
      JCudaDriver.cuMemcpyDtoD(result.deviceDataPtr, deviceDataPtr.withByteOffset(from * columns * itemSize), (to - from) * columns * itemSize)
      result
    } else {
      //todo - use JCudaDriver.cuMemcpy2D
      MatrixKernel.matrix(sliceColumns, this, from, to, result)
    }
  }

  def values(): Array[Array[T]] = {
    val data = Array.ofDim[T](rows * columns)
    copy2host(data)
    Matrix.values(rows, columns)(data)
  }

  def values(indices: Vector[Int], axis: Axis): Matrix[T] = {
    if (axis == 0) {
      MatrixKernel.matrix2(matrixRows, this, indices, new Matrix[T](indices.length, columns))
    } else {
      MatrixKernel.matrix2(matrixColumns, this, indices, new Matrix[T](rows, indices.length))
    }
  }

  def values(indices: Vector[Int], axis: Axis, result: Matrix[T]): Matrix[T] = {
    if (axis == 0) {
      MatrixKernel.matrix2_r(matrixRows, this, indices, result)
    } else {
      MatrixKernel.matrix2_r(matrixColumns, this, indices, result)
    }
  }

  def asVector(): Vector[T] = {
    return new Vector(deviceDataPtr, rows * columns)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def +(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusMatrix, this, matrix, result)
  }

  def +=(matrix: Matrix[T]): Unit = {
    MatrixKernel.matrix_r(matrixPlusMatrix, this, matrix, this)
  }

  def +(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixAddRow, this, vector, new Matrix[T](rows, columns))
  }

  def +(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddRow, this, vector, result)
  }

  def +=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixAddRow, this, vector, this)
  }

  def +|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixAddColumn, this, vector, new Matrix[T](rows, columns))
  }

  def +|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixAddColumn, this, vector, result)
  }

  def +|=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixAddColumn, this, vector, this)
  }

  def +(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Byte): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def +(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Short): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def +(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Int): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def +(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Long): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def +(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Float): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def +(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixPlusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def +(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, result)
  }

  def +=(scalar: Double): Unit = {
    MatrixKernel.matrix_r(matrixPlusScalar, this, scalar, this)
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def -(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusMatrix, this, matrix, result)
  }

  def -=(matrix: Matrix[T]): Unit = {
    MatrixKernel.matrix_r(matrixMinusMatrix, this, matrix, this)
  }

  def -(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixSubRow, this, vector, new Matrix[T](rows, columns))
  }

  def -(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubRow, this, vector, result)
  }

  def -=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixSubRow, this, vector, this)
  }

  def -|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixSubColumn, this, vector, new Matrix[T](rows, columns))
  }

  def -|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixSubColumn, this, vector, result)
  }

  def -|=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixSubColumn, this, vector, this)
  }

  def -(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Byte): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def -(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Short): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def -(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Int): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def -(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Long): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def -(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Float): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def -(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixMinusScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def -(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, result)
  }

  def -=(scalar: Double): Unit = {
    MatrixKernel.matrix_r(matrixMinusScalar, this, scalar, this)
  }

  def ***(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2(matrixDotMatrix, this, matrix, new Matrix[T](rows, matrix.columns))
  }

  def ***(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2_r(matrixDotMatrix, this, matrix, result)
  }

  def ***(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(matrixDotVector, this, vector, new Vector[T](rows))
  }

  def ***(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixDotVector, this, vector, result)
  }

  def *(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Byte): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Short): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Int): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Long): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Float): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def *(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, result)
  }

  def *=(scalar: Double): Unit = {
    MatrixKernel.matrix_r(matrixTimesScalar, this, scalar, this)
  }

  def *(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def *(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesMatrix, this, matrix, result)
  }

  def *=(matrix: Matrix[T]): Unit = {
    MatrixKernel.matrix_r(matrixTimesMatrix, this, matrix, this)
  }

  def *(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesRow, this, vector, new Matrix[T](rows, columns))
  }

  def *(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesRow, this, vector, result)
  }

  def *=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixTimesRow, this, vector, this)
  }

  def *|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixTimesColumn, this, vector, new Matrix[T](rows, columns))
  }

  def *|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixTimesColumn, this, vector, result)
  }

  def *|=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixTimesColumn, this, vector, this)
  }

  def /(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixDivRow, this, vector, new Matrix[T](rows, columns))
  }

  def /(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivRow, this, vector, result)
  }

  def /=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixDivRow, this, vector, this)
  }

  def /|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixDivColumn, this, vector, new Matrix[T](rows, columns))
  }

  def /|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivColumn, this, vector, result)
  }

  def /|=(vector: Vector[T]): Unit = {
    MatrixKernel.matrix_r(matrixDivColumn, this, vector, this)
  }

  def /(scalar: Byte): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Byte, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Byte): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(scalar: Short): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Short, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Short): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(scalar: Int): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Int, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Int): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(scalar: Long): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Long, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Long): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(scalar: Float): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Float, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Float): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(scalar: Double): Matrix[T] = {
    MatrixKernel.matrix(matrixDivScalar, this, scalar, new Matrix[T](rows, columns))
  }

  def /(scalar: Double, result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, result)
  }

  def /=(scalar: Double): Unit = {
    MatrixKernel.matrix_r(matrixDivScalar, this, scalar, this)
  }

  def /(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(matrixDivMatrix, this, matrix, new Matrix[T](rows, columns))
  }

  def /(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(matrixDivMatrix, this, matrix, result)
  }

  def /=(matrix: Matrix[T]): Unit = {
    MatrixKernel.matrix_r(matrixDivMatrix, this, matrix, this)
  }

  def *^(power: Float): Matrix[Float] = {
    if (power == 2) {
      pow2(this)
    } else {
      pow(this, power)
    }
  }

  def *^(power: Double): Matrix[Double] = {
    if (power == 2) {
      pow2d(this)
    } else {
      powd(this, power)
    }
  }

  def T: Matrix[T] = {
    MatrixKernel.matrix(transpose, this, new Matrix[T](columns, rows))
  }

  def T(result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(transpose, this, result)
  }

  def row(i: Int): Vector[T] = {
    VectorKernel.vector(getRow, this, i, new Vector[T](columns))
  }

  def row(i: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(getRow, this, i, result)
  }

  def column(j: Int): Vector[T] = {
    VectorKernel.vector(getColumn, this, j, new Vector[T](columns))
  }

  def column(j: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(getColumn, this, j, result)
  }

  def sum(): Scalar[T] = {
    Matrix.sum(this)
  }

  def sum(axis: Axis): Vector[T] = {
    Matrix.sum(this, axis)
  }

  def mean(): Scalar[Float] = {
    Matrix.mean(this)
  }

  def meand(): Scalar[Double] = {
    Matrix.meand(this)
  }

  def mean(axis: Axis): Vector[Float] = {
    Matrix.mean(this, axis)
  }

  def meand(axis: Axis): Vector[Double] = {
    Matrix.meand(this, axis)
  }

  def std(): Scalar[Float] = {
    Matrix.std(this)
  }

  def stdd(): Scalar[Double] = {
    Matrix.stdd(this)
  }

  def std(axis: Axis): Vector[Float] = {
    Matrix.std(this, axis)
  }

  def stdd(axis: Axis): Vector[Double] = {
    Matrix.stdd(this, axis)
  }

  def max(): Scalar[T] = {
    Matrix.max(this)
  }

  def min(): Scalar[T] = {
    Matrix.min(this)
  }

  def argmax(): Vector[Int] = {
    Matrix.argmax(this)
  }

  def argmin(): Vector[Int] = {
    Matrix.argmin(this)
  }

  def indices(axis: Axis): Matrix[Int] = {
    if (axis == 0) {
      MatrixKernel.matrix(rowsIndices, this, new Matrix[Int](rows, columns))
    } else {
      MatrixKernel.matrix(columnsIndices, this, new Matrix[Int](rows, columns))
    }
  }

  def reverse(axis: Axis): Matrix[T] = {
    ??? //todo
  }

}

object Matrix {

  //kernels for matrix instances
  private lazy val matrixPlusMatrix = new KernelTemplate(new Matrix2MatrixElementwise("plus"))
  private lazy val matrixPlusScalar = new KernelTemplate(new  Matrix2ScalarElementwise("plus"))
  private lazy val getColumn = new KernelTemplate(new GetColumn)
  private lazy val updateColumn = new KernelTemplate(new UpdateColumn)
  private lazy val sliceColumns = new KernelTemplate(new SliceColumns)
  private lazy val matrixDivScalar = new KernelTemplate(new  Matrix2ScalarElementwise("div"))
  private lazy val matrixDivMatrix = new KernelTemplate(new Matrix2MatrixElementwise("div"))
  private lazy val matrixTimesMatrix = new KernelTemplate(new Matrix2MatrixElementwise("times"))
  private lazy val matrixDotMatrix = new KernelTemplate(new MatrixDotMatrix)
  private lazy val matrixTimesScalar = new KernelTemplate(new  Matrix2ScalarElementwise("times"))
  private lazy val matrixDotVector = new KernelTemplate(new MatrixDotVector)
  private lazy val getRow = new KernelTemplate(new GetRow)
  private lazy val matrixMinusMatrix = new KernelTemplate(new Matrix2MatrixElementwise("minus"))
  private lazy val matrixMinusScalar = new KernelTemplate(new  Matrix2ScalarElementwise("minus"))
  private lazy val transpose = new KernelTemplate(new Transpose)
  private lazy val matrixAddRow = new KernelTemplate(new MatrixAddRow)
  private lazy val matrixAddColumn = new KernelTemplate(new MatrixAddColumn)
  private lazy val matrixSubRow = new KernelTemplate(new MatrixSubRow)
  private lazy val matrixSubColumn = new KernelTemplate(new MatrixSubColumn)
  private lazy val matrixTimesRow = new KernelTemplate(new MatrixTimesRow)
  private lazy val matrixTimesColumn = new KernelTemplate(new MatrixTimesColumn)
  private lazy val matrixDivRow = new KernelTemplate(new MatrixDivRow)
  private lazy val matrixDivColumn = new KernelTemplate(new MatrixDivColumn)
  private lazy val getColumnsValues = new KernelTemplate(new GetColumnsValues)
  private lazy val setColumnsValues = new KernelTemplate(new SetColumnsValues)
  private lazy val setColumnsValue = new KernelTemplate(new SetColumnsValue)
  private lazy val getRowsValues = new KernelTemplate(new GetRowsValues)
  private lazy val setRowsValues = new KernelTemplate(new SetRowsValues)
  private lazy val setRowsValue = new KernelTemplate(new SetRowsValue)
  private lazy val getIndexedValues = new KernelTemplate(new GetIndexedValues)
  private lazy val setIndexedValues = new KernelTemplate(new SetIndexedValues)
  private lazy val setIndexedValue = new KernelTemplate(new SetIndexedValue)
  private lazy val columnsIndices = new KernelTemplate(new ColumnsIndices)
  private lazy val rowsIndices = new KernelTemplate(new RowsIndices)
  private lazy val matrixRows = new KernelTemplate(new MatrixRows)
  private lazy val matrixColumns = new KernelTemplate(new MatrixColumns)

  //kernels for matrix functions
  private lazy val sum = new KernelTemplate(new Sum)
  private lazy val sumRows = new KernelTemplate(new SumRows)
  private lazy val sumColumns = new KernelTemplate(new SumColumns)
  private lazy val mean = new KernelTemplate(new Mean("mean"))
  private lazy val meand = new KernelTemplate(new Mean("meand"))
  private lazy val rowsMean = new KernelTemplate(new RowsMean("rowsMean"))
  private lazy val rowsMeand = new KernelTemplate(new RowsMean("rowsMeand"))
  private lazy val columnsMean = new KernelTemplate(new ColumnsMean("columnsMean"))
  private lazy val columnsMeand = new KernelTemplate(new ColumnsMean("columnsMeand"))
  private lazy val std = new KernelTemplate(new Std("math_std"))
  private lazy val stdd = new KernelTemplate(new Std("math_stdd"))
  private lazy val rowsStd = new KernelTemplate(new RowsStd("rowsStd"))
  private lazy val rowsStdd = new KernelTemplate(new RowsStd("rowsStdd"))
  private lazy val columnsStd = new KernelTemplate(new ColumnsStd("columnsStd"))
  private lazy val columnsStdd = new KernelTemplate(new ColumnsStd("columnsStdd"))
  private lazy val matrixMin = new KernelTemplate(new MatrixMin)
  private lazy val matrixMax = new KernelTemplate(new MatrixMax)
  private lazy val matrixMinColumn = new KernelTemplate(new MatrixMinColumn)
  private lazy val matrixMaxColumn = new KernelTemplate(new MatrixMaxColumn())
  private lazy val matrixArgminColumn = new KernelTemplate(new MatrixArgminColumn)
  private lazy val matrixArgmaxColumn = new KernelTemplate(new MatrixArgmaxColumn)
  private lazy val matrixBincount = new MatrixBincount

  //kernels for matrix cas functions with one operand 
  private lazy val casScalarThresholdScalarOperand                 = new KernelTemplate(new Cas("Cas_scalar", "cas_scalar_threshold_scalar_operand"))
  private lazy val casScalarThresholdRowOperand                    = new KernelTemplate(new Cas("Cas_scalar", "cas_scalar_threshold_row_operand"))
  private lazy val casScalarThresholdColumnOperand                 = new KernelTemplate(new Cas("Cas_scalar", "cas_scalar_threshold_column_operand"))
  private lazy val casScalarThresholdMatrixOperand                 = new KernelTemplate(new Cas("Cas_scalar", "cas_scalar_threshold_matrix_operand"))
  private lazy val casRowThresholdScalarOperand                    = new KernelTemplate(new Cas("Cas_vector", "cas_row_threshold_scalar_operand"))
  private lazy val casColumnThresholdScalarOperand                 = new KernelTemplate(new Cas("Cas_vector", "cas_column_threshold_scalar_operand"))
  private lazy val casRowThresholdRowOperand                       = new KernelTemplate(new Cas("Cas_vector", "cas_row_threshold_row_operand"))
  private lazy val casColumnThresholdColumnOperand                 = new KernelTemplate(new Cas("Cas_vector", "cas_column_threshold_column_operand"))
  private lazy val casRowThresholdMatrixOperand                    = new KernelTemplate(new Cas("Cas_vector", "cas_row_threshold_matrix_operand"))
  private lazy val casColumnThresholdMatrixOperand                 = new KernelTemplate(new Cas("Cas_vector", "cas_column_threshold_matrix_operand"))
  private lazy val casMatrixThresholdScalarOperand                 = new KernelTemplate(new Cas("Cas_matrix", "cas_matrix_threshold_scalar_operand"))
  private lazy val casMatrixThresholdRowOperand                    = new KernelTemplate(new Cas("Cas_matrix", "cas_matrix_threshold_row_operand"))
  private lazy val casMatrixThresholdColumnOperand                 = new KernelTemplate(new Cas("Cas_matrix", "cas_matrix_threshold_column_operand"))
  private lazy val casMatrixThresholdMatrixOperand                 = new KernelTemplate(new Cas("Cas_matrix", "cas_matrix_threshold_matrix_operand"))

  //kernels for matrix cas functions with two operands
  private lazy val casScalarThresholdScalarOperand1ScalarOperand2  = new KernelTemplate(new Cas("Cas_scalar2", "cas_scalar_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casScalarThresholdRowOperand1RowOperand2        = new KernelTemplate(new Cas("Cas_scalar2", "cas_scalar_threshold_row_operand1_row_operand2"))
  private lazy val casScalarThresholdColumnOperand1ColumnOperand2  = new KernelTemplate(new Cas("Cas_scalar2", "cas_scalar_threshold_column_operand1_column_operand2"))
  private lazy val casScalarThresholdMatrixOperand1MatrixOperand2  = new KernelTemplate(new Cas("Cas_scalar2", "cas_scalar_threshold_matrix_operand1_matrix_operand2"))
  private lazy val casRowThresholdScalarOperand1ScalarOperand2     = new KernelTemplate(new Cas("Cas_vector2", "cas_row_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casColumnThresholdScalarOperand1ScalarOperand2  = new KernelTemplate(new Cas("Cas_vector2", "cas_column_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casRowThresholdRowOperand1RowOperand2           = new KernelTemplate(new Cas("Cas_vector2", "cas_row_threshold_row_operand1_row_operand2"))
  private lazy val casColumnThresholdColumnOperand1ColumnOperand2  = new KernelTemplate(new Cas("Cas_vector2", "cas_column_threshold_column_operand1_column_operand2"))
  private lazy val casRowThresholdMatrixOperand1MatrixOperand2     = new KernelTemplate(new Cas("Cas_vector2", "cas_row_threshold_matrix_operand1_matrix_operand2"))
  private lazy val casColumnThresholdMatrixOperand1MatrixOperand2  = new KernelTemplate(new Cas("Cas_vector2", "cas_column_threshold_matrix_operand1_matrix_operand2"))
  private lazy val casMatrixThresholdScalarOperand1ScalarOperand2  = new KernelTemplate(new Cas("Cas_matrix2", "cas_matrix_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casMatrixThresholdRowOperand1RowOperand2        = new KernelTemplate(new Cas("Cas_matrix2", "cas_matrix_threshold_row_operand1_row_operand2"))
  private lazy val casMatrixThresholdColumnOperand1ColumnOperand2  = new KernelTemplate(new Cas("Cas_matrix2", "cas_matrix_threshold_column_operand1_column_operand2"))
  private lazy val casMatrixThresholdMatrixOperand1MatrixOperand2  = new KernelTemplate(new Cas("Cas_matrix2", "cas_matrix_threshold_matrix_operand1_matrix_operand2"))

  //kernels for matrix cas functions with two operands and indexed arguments
  private lazy val casScalarThresholdIndexedOperand                 = new KernelTemplate(new CasIndexed("Cas_indexed_scalar", "cas_scalar_threshold_indexed_operand"))
  private lazy val casIndexedThresholdScalarOperand                 = new KernelTemplate(new CasIndexed("Cas_indexed_scalar", "cas_indexed_threshold_scalar_operand"))
  private lazy val casIndexedThresholdIndexedOperand                 = new KernelTemplate(new CasIndexed("Cas_indexed_indexed", "cas_indexed_threshold_indexed_operand"))
  private lazy val casMatrixThresholdIndexedOperand                 = new KernelTemplate(new CasIndexed("Cas_indexed_matrix", "cas_matrix_threshold_indexed_operand"))
  private lazy val casIndexedThresholdMatrixOperand                 = new KernelTemplate(new CasIndexed("Cas_indexed_matrix", "cas_indexed_threshold_matrix_operand"))

  //kernels for matrix cas functions with two operands and indexed arguments
  private lazy val casScalarThresholdIndexedOperand1IndexedOperand2  = new KernelTemplate(new CasIndexed("Cas_indexed_scalar2", "cas_scalar_threshold_indexed_operand1_indexed_operand2"))
  private lazy val casIndexedThresholdScalarOperand1ScalarOperand2  = new KernelTemplate(new CasIndexed("Cas_indexed_scalar2", "cas_indexed_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casIndexedThresholdIndexedOperand1IndexedOperand2  = new KernelTemplate(new CasIndexed("Cas_indexed_indexed2", "cas_indexed_threshold_indexed_operand1_indexed_operand2"))
  private lazy val casMatrixThresholdIndexedOperand1IndexedOperand2  = new KernelTemplate(new CasIndexed("Cas_indexed_matrix2", "cas_matrix_threshold_indexed_operand1_indexed_operand2"))
  private lazy val casIndexedThresholdMatrixOperand1MatrixOperand2  = new KernelTemplate(new CasIndexed("Cas_indexed_matrix2", "cas_indexed_threshold_matrix_operand1_matrix_operand2"))

  def apply[T: ClassTag](rows: Int, columns: Int): MatrixBuilder[T] = {
    new MatrixBuilder[T](rows, columns)
  }

  def of[T: ClassTag](values: Array[Array[T]]): Matrix[T] = {
    val rows = Matrix.rows(values)
    val columns = Matrix.columns(values)
    new Matrix(data(rows, columns)(values), rows, columns)
  }

  def of[T: ClassTag](data: Array[T], rows: Int, columns: Int): Matrix[T] = {
    new Matrix(data, rows, columns)
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

  def sum[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    ScalarKernel.scalar_r(sum, matrix, result)
  }

  def sum[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    ScalarKernel.scalar(sum, matrix)
  }

  def sum[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      sumRows(matrix, result)
    } else {
      sumColumns(matrix, result)
    }
  }

  def sum[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[T] = {
    if (axis == 0) {
      sumRows(matrix)
    } else {
      sumColumns(matrix)
    }
  }

  def sumRows[T: ClassTag](matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(sumRows, matrix, result)
  }

  def sumRows[T: ClassTag](matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(sumRows, matrix, new Vector[T](matrix.columns))
  }

  def sumColumns[T: ClassTag](matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(sumColumns, matrix, result)
  }

  def sumColumns[T: ClassTag](matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(sumColumns, matrix, new Vector[T](matrix.rows))
  }

  def mean[T: ClassTag](matrix: Matrix[T], result: Scalar[Float]): Scalar[Float] = {
    ScalarKernel.scalar_r(mean, matrix, result)
  }

  def mean[T: ClassTag](matrix: Matrix[T]): Scalar[Float] = {
    ScalarKernel.scalar(mean, matrix)
  }

  def mean[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Float]): Vector[Float] = {
    if (axis == 0) {
      columnsMean(matrix, result)
    } else {
      rowsMean(matrix, result)
    }
  }

  def mean[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Float] = {
    if (axis == 0) {
      columnsMean(matrix)
    } else {
      rowsMean(matrix)
    }
  }

  def rowsMean[T: ClassTag](matrix: Matrix[T], result: Vector[Float]): Vector[Float] = {
    VectorKernel.vector_r(rowsMean, matrix, result)
  }

  def rowsMean[T: ClassTag](matrix: Matrix[T]): Vector[Float] = {
    VectorKernel.vector(rowsMean, matrix, new Vector[Float](matrix.rows))
  }

  def columnsMean[T: ClassTag](matrix: Matrix[T], result: Vector[Float]): Vector[Float] = {
    VectorKernel.vector_r(columnsMean, matrix, result)
  }

  def columnsMean[T: ClassTag](matrix: Matrix[T]): Vector[Float] = {
    VectorKernel.vector(columnsMean, matrix, new Vector[Float](matrix.columns))
  }

  def meand[T: ClassTag](matrix: Matrix[T], result: Scalar[Double]): Scalar[Double] = {
    ScalarKernel.scalar_r(meand, matrix, result)
  }

  def meand[T: ClassTag](matrix: Matrix[T]): Scalar[Double] = {
    ScalarKernel.scalar(meand, matrix)
  }

  def meand[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Double]): Vector[Double] = {
    if (axis == 0) {
      columnsMeand(matrix, result)
    } else {
      rowsMeand(matrix, result)
    }
  }

  def meand[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Double] = {
    if (axis == 0) {
      columnsMeand(matrix)
    } else {
      rowsMeand(matrix)
    }
  }

  def rowsMeand[T: ClassTag](matrix: Matrix[T], result: Vector[Double]): Vector[Double] = {
    VectorKernel.vector_r(rowsMeand, matrix, result)
  }

  def rowsMeand[T: ClassTag](matrix: Matrix[T]): Vector[Double] = {
    VectorKernel.vector(rowsMeand, matrix, new Vector[Double](matrix.rows))
  }

  def columnsMeand[T: ClassTag](matrix: Matrix[T], result: Vector[Double]): Vector[Double] = {
    VectorKernel.vector_r(columnsMeand, matrix, result)
  }

  def columnsMeand[T: ClassTag](matrix: Matrix[T]): Vector[Double] = {
    VectorKernel.vector(columnsMeand, matrix, new Vector[Double](matrix.columns))
  }

  def std[T: ClassTag](matrix: Matrix[T], result: Scalar[Float]): Scalar[Float] = {
    ScalarKernel.scalar_r(std, matrix, result)
  }

  def std[T: ClassTag](matrix: Matrix[T]): Scalar[Float] = {
    ScalarKernel.scalar(std, matrix)
  }

  def std[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Float]): Vector[Float] = {
    if (axis == 0) {
      columnsStd(matrix, result)
    } else {
      rowsStd(matrix, result)
    }
  }

  def std[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Float] = {
    if (axis == 0) {
      columnsStd(matrix)
    } else {
      rowsStd(matrix)
    }
  }

  def rowsStd[T: ClassTag](matrix: Matrix[T], result: Vector[Float]): Vector[Float] = {
    VectorKernel.vector_r(rowsStd, matrix, result)
  }

  def rowsStd[T: ClassTag](matrix: Matrix[T]): Vector[Float] = {
    VectorKernel.vector(rowsStd, matrix, new Vector[Float](matrix.rows))
  }

  def columnsStd[T: ClassTag](matrix: Matrix[T], result: Vector[Float]): Vector[Float] = {
    VectorKernel.vector_r(columnsStd, matrix, result)
  }

  def columnsStd[T: ClassTag](matrix: Matrix[T]): Vector[Float] = {
    VectorKernel.vector(columnsStd, matrix, new Vector[Float](matrix.columns))
  }

  def stdd[T: ClassTag](matrix: Matrix[T], result: Scalar[Double]): Scalar[Double] = {
    ScalarKernel.scalar_r(stdd, matrix, result)
  }

  def stdd[T: ClassTag](matrix: Matrix[T]): Scalar[Double] = {
    ScalarKernel.scalar(stdd, matrix)
  }

  def stdd[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Double]): Vector[Double] = {
    if (axis == 0) {
      columnsStdd(matrix, result)
    } else {
      rowsStdd(matrix, result)
    }
  }

  def stdd[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Double] = {
    if (axis == 0) {
      columnsStdd(matrix)
    } else {
      rowsStdd(matrix)
    }
  }

  def rowsStdd[T: ClassTag](matrix: Matrix[T], result: Vector[Double]): Vector[Double] = {
    VectorKernel.vector_r(rowsStdd, matrix, result)
  }

  def rowsStdd[T: ClassTag](matrix: Matrix[T]): Vector[Double] = {
    VectorKernel.vector(rowsStdd, matrix, new Vector[Double](matrix.rows))
  }

  def columnsStdd[T: ClassTag](matrix: Matrix[T], result: Vector[Double]): Vector[Double] = {
    VectorKernel.vector_r(columnsStdd, matrix, result)
  }

  def columnsStdd[T: ClassTag](matrix: Matrix[T]): Vector[Double] = {
    VectorKernel.vector(columnsStdd, matrix, new Vector[Double](matrix.columns))
  }

  def min[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(matrixMin, matrix, result)
  }

  def min[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    scalar(matrixMin, matrix)
  }

  def min[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixMinColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixMinColumn, matrix, result)
    }
  }

  def min[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector(matrixMinColumn, matrix.T, new Vector[T](matrix.columns))
    } else {
      VectorKernel.vector(matrixMinColumn, matrix, new Vector[T](matrix.rows))
    }
  }

  def max[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(matrixMax, matrix, result)
  }

  def max[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    scalar(matrixMax, matrix)
  }

  def max[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixMaxColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixMaxColumn, matrix, result)
    }
  }

  def max[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector(matrixMaxColumn, matrix.T, new Vector[T](matrix.columns))
    } else {
      VectorKernel.vector(matrixMaxColumn, matrix, new Vector[T](matrix.rows))
    }
  }

  def argmin[T: ClassTag](matrix: Matrix[T], result: Vector[Int]): Vector[Int] = {
    argmin(matrix, axis = 1, result)
  }

  def argmin[T: ClassTag](matrix: Matrix[T]): Vector[Int] = {
    argmin(matrix, axis = 1)
  }

  def argmin[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Int]): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixArgminColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixArgminColumn, matrix, result)
    }
  }

  def argmin[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector(matrixArgminColumn, matrix.T, new Vector[Int](matrix.columns))
    } else {
      VectorKernel.vector(matrixArgminColumn, matrix, new Vector[Int](matrix.rows))
    }
  }

  def argmax[T: ClassTag](matrix: Matrix[T], result: Vector[Int]): Vector[Int] = {
    argmax(matrix, axis = 1, result)
  }

  def argmax[T: ClassTag](matrix: Matrix[T]): Vector[Int] = {
    argmax(matrix, axis = 1)
  }

  def argmax[T: ClassTag](matrix: Matrix[T], axis: Axis, result: Vector[Int]): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixArgmaxColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixArgmaxColumn, matrix, result)
    }
  }

  def argmax[T: ClassTag](matrix: Matrix[T], axis: Axis): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector(matrixArgmaxColumn, matrix.T, new Vector[Int](matrix.columns))
    } else {
      VectorKernel.vector(matrixArgmaxColumn, matrix, new Vector[Int](matrix.rows))
    }
  }

  def sort[T: ClassTag](matrix: Matrix[T], context: KeySortContext): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns, context)
    matrix
  }

  def sort[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns)
    matrix
  }

  def sort[T: ClassTag](matrix: Matrix[T], order: SortOrder, context: KeySortContext): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns, context)
    if (order == SortOrder.ASC) {
      matrix
    } else {
      matrix.reverse(1)
    }
  }

  def sort[T: ClassTag](matrix: Matrix[T], order: SortOrder): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns)
    if (order == SortOrder.ASC) {
      matrix
    } else {
      matrix.reverse(1)
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], context: KeyValueSortContext): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns, context)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V]): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], order: SortOrder, context: KeyValueSortContext): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns, context)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(1), values.reverse(1))
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], order: SortOrder): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(1), values.reverse(1))
    }
  }

  def argsort[T: ClassTag](matrix: Matrix[T], context: KeyValueSortContext): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, context)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T]): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T], order: SortOrder, context: KeyValueSortContext): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, order, context)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T], order: SortOrder): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, order)
    indices
  }

  def bincount(matrix: Matrix[Int], axis: Axis): Matrix[Int] = {
    val maxValue = max(matrix).value()
    bincount(matrix, maxValue, axis)
  }

  def bincount(matrix: Matrix[Int], maxValue: Int, axis: Axis, result: Matrix[Int]): Matrix[Int] = {
    if (axis == 0) {
      MatrixKernel.matrix_r(matrixBincount, matrix.T, maxValue, result).T
    } else {
      MatrixKernel.matrix_r(matrixBincount, matrix, maxValue, result)
    }
  }

  def bincount(matrix: Matrix[Int], maxValue: Int, axis: Axis): Matrix[Int] = {
    if (axis == 0) {
      MatrixKernel.matrix(matrixBincount, matrix.T, maxValue, new Matrix[Int](matrix.columns, maxValue + 1)).T
    } else {
      MatrixKernel.matrix(matrixBincount, matrix, maxValue, new Matrix[Int](matrix.rows, maxValue + 1))
    }
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

  object Cas {

    object Indexed {

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casScalarThresholdIndexedOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casScalarThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: T, rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdScalarOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdIndexedOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casMatrixThresholdIndexedOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casMatrixThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Matrix[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdMatrixOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casScalarThresholdIndexedOperand, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casScalarThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdScalarOperand, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdScalarOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdIndexedOperand, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casMatrixThresholdIndexedOperand, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casMatrixThresholdIndexedOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdMatrixOperand.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator, operand), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator, operand, rows, columns, result)
      }
      
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casScalarThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casScalarThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casScalarThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casScalarThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: T, axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: T, axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Matrix[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand: Matrix[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdScalarOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casMatrixThresholdRowOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casMatrixThresholdColumnOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdMatrixOperand, matrix, comparator, threshold, Operator.ASSIGN, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdScalarOperand, matrix, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdScalarOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casScalarThresholdRowOperand, matrix, comparator, threshold, operator, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casScalarThresholdColumnOperand, matrix, comparator, threshold, operator, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casScalarThresholdRowOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casScalarThresholdColumnOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdScalarOperand, matrix, comparator, threshold, operator, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdScalarOperand, matrix, comparator, threshold, operator, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdScalarOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdScalarOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdRowOperand, matrix, comparator, threshold, operator, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdColumnOperand, matrix, comparator, threshold, operator, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdRowOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdColumnOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdScalarOperand, matrix, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdScalarOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdRowOperand, matrix, comparator, threshold, operator, operand, result)
      } else {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdColumnOperand, matrix, comparator, threshold, operator, operand, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casMatrixThresholdRowOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casMatrixThresholdColumnOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdMatrixOperand, matrix, comparator, threshold, operator, operand, new Matrix[T](matrix.rows, matrix.columns))
    }

  }

  object Cas2 {

    object Indexed {

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casScalarThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casScalarThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdScalarOperand1ScalarOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casMatrixThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Vector[T], operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casMatrixThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Matrix[T], operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Matrix[T], operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdMatrixOperand1MatrixOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casScalarThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casScalarThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdScalarOperand1ScalarOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casMatrixThresholdIndexedOperand1IndexedOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casMatrixThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[T]): Matrix[T] = {
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(casIndexedThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }

      def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int]): Matrix[T] = {
        val kernel = casIndexedThresholdMatrixOperand1MatrixOperand2.kernel[T]
        val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Matrix[T](matrix.rows, matrix.columns))
        result.copyFrom(matrix, matrix.rows * matrix.columns)
        CasIndexed.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
      }
      
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: T, operand2: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: T, operand2: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casScalarThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casScalarThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casScalarThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casScalarThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Matrix[T], operand2: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operand1: Matrix[T], operand2: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Matrix[T], operand2: Matrix[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operand1: Matrix[T], operand2: Matrix[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: T, operand2: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: T, operand2: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Vector[T], operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Vector[T], operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casMatrixThresholdRowOperand1RowOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casMatrixThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Matrix[T], operand2: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operand1: Matrix[T], operand2: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casScalarThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casScalarThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casScalarThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casScalarThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casScalarThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casScalarThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casRowThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casColumnThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casRowThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casColumnThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdScalarOperand1ScalarOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis, result: Matrix[T]): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      } else {
        kernel.matrix.Cas.matrix_r(casMatrixThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], axis: Axis): Matrix[T] = {
      if (axis == 0) {
        kernel.matrix.Cas.matrix(casMatrixThresholdRowOperand1RowOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      } else {
        kernel.matrix.Cas.matrix(casMatrixThresholdColumnOperand1ColumnOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
      }
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix_r(casMatrixThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T]): Matrix[T] = {
      kernel.matrix.Cas.matrix(casMatrixThresholdMatrixOperand1MatrixOperand2, matrix, comparator, threshold, operator1, operand1, operator2, operand2, new Matrix[T](matrix.rows, matrix.columns))
    }

  }

}
