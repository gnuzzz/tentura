package ru.albemuth.tentura.tensor

import Vector._
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, sizeOf}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.MathFunctions.{pow, pow2, pow2d, powd}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Vector[T: ClassTag](override val deviceDataPtr: CUdeviceptr, val length: Int) extends DeviceVar[T] {

  def this(l: Int) {
    this(devicePtr(l), l)
  }

  def this(data: Array[T]) {
    this(devicePtr(data.length), data.length)
    copy2device(data)
  }

  def apply(i: Int): Scalar[T] = {
    result("apply", i, new Scalar(deviceDataPtr.withByteOffset(i * sizeOf())))
  }

  def apply(from: Int, to: Int): Vector[T] = {
    result("apply", (from, to), new Vector(deviceDataPtr.withByteOffset(from * sizeOf()), to - from))
  }

  def update(i: Int, value: T): Unit = {
    copy2device(value, i)
  }

  def update(values: Vector[T]): Unit = {
    copy(values, values.length)
  }

  def update(from: Int, to: Int, values: Vector[T]): Unit = {
    copy(values, 0, from, to - from)
  }

  def slice(from: Int, to: Int): Vector[T] = {
    val r = this.result(Vector.slice, (from, to), new Vector[T](to - from))
    r.copy(this, from, 0, to - from)
    r
  }

  def slice(from: Int, to: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(Vector.slice, this, from, to, result)
  }

  def concat(vector: Vector[T]): Vector[T] = {
    ??? //todo
  }

  def values(): Array[T] = {
    val data = Array.ofDim[T](length)
    copy2host(data)
    data
  }

  def values(indices: Vector[Int]): Vector[T] = {
    VectorKernel.vector2(Vector.values, this, indices, new Vector[T](indices.length))
  }

  def values(indices: Vector[Int], result: Vector[T]): Vector[T] = {
    VectorKernel.vector2_r(Vector.values, this, indices, result)
  }

  def values(indices: Matrix[Int]): Matrix[T] = {
    MatrixKernel.matrix2(vectorValuesMatrix, this, indices, new Matrix[T](indices.rows, indices.columns))
  }

  def values(indices: Matrix[Int], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2_r(vectorValuesMatrix, this, indices, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowAddMatrix, this, matrix, result)
  }

  def +|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnAddMatrix, this, matrix, result)
  }

  def +(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorAddVector, this, vector, new Vector[T](length))
  }

  def +(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddVector, this, vector, result)
  }

  def +(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowSubMatrix, this, matrix, result)
  }

  def -|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnSubMatrix, this, matrix, result)
  }

  def -(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorSubVector, this, vector, new Vector[T](length))
  }

  def -(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubVector, this, vector, result)
  }

  def -(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def *(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *(vector: Vector[T]): Scalar[T] = {
    ScalarKernel.scalar(vectorDotVector, this, vector, new Scalar[T]())
  }

  def *(vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    ScalarKernel.scalar_r(vectorDotVector, this, vector, result)
  }

  def *(matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(vectorMulMatrix, this, matrix, new Vector[T](matrix.columns))
  }

  def *(matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorMulMatrix, this, matrix, result)
  }

  def |*(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(columnDotRow, this, vector, new Matrix[T](length, vector.length))
  }

  def |*(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnDotRow, this, vector, result)
  }

  def :*(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorTimesVector, this, vector, new Vector[T](length))
  }

  def :*(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesVector, this, vector, result)
  }

  def /(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def :/(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorDivVector, this, vector, new Vector[T](length))
  }

  def :/(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivVector, this, vector, result)
  }

  def ^(power: Float): Vector[Float] = {
    if (power == 2) {
      pow2(this)
    } else {
      pow(this, power)
    }
  }

  def ^(power: Double): Vector[Double] = {
    if (power == 2) {
      pow2d(this)
    } else {
      powd(this, power)
    }
  }

  def sum(): Scalar[T] = {
    VectorFunctions.sum(this)
  }

  def max(): Scalar[T] = {
    VectorFunctions.max(this)
  }

  def min(): Scalar[T] = {
    VectorFunctions.min(this)
  }

  def argmax(): Scalar[Int] = {
    VectorFunctions.argmax(this)
  }

  def argmin: Scalar[Int] = {
    VectorFunctions.argmin(this)
  }

  def indices(): Vector[Int] = {
    VectorKernel.vector(Vector.indices, this, new Vector[Int](length))
  }

  def reverse(): Vector[T] = {
    VectorKernel.vector(Vector.reverse, this, new Vector[T](length))
  }

  def reverse(result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(Vector.reverse, this, result)
  }

}

object Vector {

  lazy val vectorAddVector = new KernelTemplate(new VectorAddVector)
  lazy val vectorAddScalar = new KernelTemplate(new VectorAddScalar)
  lazy val vectorSubVector = new KernelTemplate(new VectorSubVector)
  lazy val vectorSubScalar = new KernelTemplate(new VectorSubScalar)
  lazy val vectorTimesScalar = new KernelTemplate(new VectorTimesScalar)
  lazy val vectorDotVector = new KernelTemplate(new VectorDotVector)
  lazy val vectorMulMatrix = new KernelTemplate(new VectorTimesMatrix)
  lazy val columnDotRow = new KernelTemplate(new ColumnDotRow)
  lazy val vectorDivVector = new KernelTemplate(new VectorDivVector)
  lazy val vectorTimesVector = new KernelTemplate(new VectorTimesVector)
  lazy val vectorDivScalar = new KernelTemplate(new VectorDivScalar)
  lazy val sum = new KernelTemplate(new Sum)
  lazy val rowAddMatrix = new KernelTemplate(new RowAddMatrix)
  lazy val columnAddMatrix = new KernelTemplate(new ColumnAddMatrix)
  lazy val rowSubMatrix = new KernelTemplate(new RowSubMatrix)
  lazy val columnSubMatrix = new KernelTemplate(new ColumnSubMatrix)
  lazy val value = new KernelTemplate(new Value)
  lazy val slice = new KernelTemplate(new Slice)
  lazy val values = new KernelTemplate(new Values)
  lazy val indices = new KernelTemplate(new Indices)
  lazy val vectorValuesMatrix = new KernelTemplate(new VectorValuesMatrix)
  lazy val reverse = new KernelTemplate(new Reverse())

  def apply[T: ClassTag](length: Int): VectorBuilder[T] = new VectorBuilder[T](length)

  def of[T: ClassTag](values: Array[T]): Vector[T] = {
    new Vector(values)
  }

  class VectorBuilder[T: ClassTag](length: Int) {

    def of(value: => T): Vector[T] = {
      new Vector(Array.fill(length)(value))
    }

  }

}