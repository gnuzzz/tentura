package ru.albemuth.tentura.tensor

import Vector._
import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, pointer}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.MathFunctions.{pow, pow2, pow2d, powd}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Vector[T: ClassTag](override protected[tensor] val data: Array[T]) extends DeviceVar[T] {

  override protected[tensor] val dataPointer: Pointer = pointer(data)
  override protected[tensor] val deviceDataPtr: CUdeviceptr = devicePtr(data)
  val length: Int = data.length

  copy2device()

  def this(l: Int) {
    this(new Array[T](l))
  }

  def apply(i: Int): Scalar[T] = {
    ScalarKernel.scalar(value, this, i)
  }

  def slice(from: Int, to: Int): Vector[T] = {
    VectorKernel.vector(vectorSlice, this, from, to, new Vector[T](to - from))
  }

  def values(): Array[T] = {
    val ret = new Array[T](length)
    System.arraycopy(data, 0, ret, 0, length)
    ret
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(vectorRowAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(vectorColumnAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorAddVector, this, vector, new Vector[T](length))
  }

  def +(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(vectorRowSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(vectorColumnSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorSubVector, this, vector, new Vector[T](length))
  }

  def -(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorMulScalar, this, scalar, new Vector[T](length))
  }

  def *(vector: Vector[T]): Scalar[T] = {
    ScalarKernel.scalar(vectorMulVector, this, vector)
  }

  def *(matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(vectorMulMatrix, this, matrix, new Vector[T](matrix.columns))
  }

  def **(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(vectorMatrixMulVector, this, vector, new Matrix[T](length, vector.length))
  }

  def :*(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorElementWiseMulVector, this, vector, new Vector[T](length))
  }

  def /(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def :/(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorElementWiseDivVector, this, vector, new Vector[T](length))
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

}

object Vector {

  lazy val vectorAddVector = new KernelTemplate(new VectorAddVector)
  lazy val vectorAddScalar = new KernelTemplate(new VectorAddScalar)
  lazy val vectorSubVector = new KernelTemplate(new VectorSubVector)
  lazy val vectorSubScalar = new KernelTemplate(new VectorSubScalar)
  lazy val vectorMulScalar = new KernelTemplate(new VectorMulScalar)
  lazy val vectorMulVector = new KernelTemplate(new VectorMulVector)
  lazy val vectorMulMatrix = new KernelTemplate(new VectorMulMatrix)
  lazy val vectorMatrixMulVector = new KernelTemplate(new VectorMatrixMulVector)
  lazy val vectorElementWiseDivVector = new KernelTemplate(new VectorElementWiseDivVector)
  lazy val vectorElementWiseMulVector = new KernelTemplate(new VectorElementWiseMulVector)
  lazy val vectorDivScalar = new KernelTemplate(new VectorDivScalar)
  lazy val vectorSum = new KernelTemplate(new VectorSum)
  lazy val vectorRowAddMatrix = new KernelTemplate(new VectorRowAddMatrix)
  lazy val vectorColumnAddMatrix = new KernelTemplate(new VectorColumnAddMatrix)
  lazy val vectorRowSubMatrix = new KernelTemplate(new VectorRowSubMatrix)
  lazy val vectorColumnSubMatrix = new KernelTemplate(new VectorColumnSubMatrix)
  lazy val value = new KernelTemplate(new Value)
  lazy val vectorSlice = new KernelTemplate(new Slice)

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