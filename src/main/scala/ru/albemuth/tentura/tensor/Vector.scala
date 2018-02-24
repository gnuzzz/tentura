package ru.albemuth.tentura.tensor

import Vector._
import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, pointer}
import ru.albemuth.tentura.tensor.kernel._

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

  def apply(i: Int): T = data(i)

  def values(): Array[T] = {
    val ret = new Array[T](length)
    System.arraycopy(data, 0, ret, 0, length)
    ret
  }

  protected[tensor] def result(kernel: VectorKernel, resultKey: Any, result: => Vector[T]): Vector[T] = {
    resultsCache.result[T, Vector[T]](kernel, resultKey, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](vectorRowAddMatrix, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    vectorRowAddMatrix.launch(params, result)

    result
  }

  def +|(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](vectorColumnAddMatrix, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    vectorColumnAddMatrix.launch(params, result)

    result
  }

  def +(vector: Vector[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorAddVector, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorAddVector.launch(params, result)

    result
  }

  def +(scalar: Float): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorAddScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorAddScalar.launch(params, result)

    result
  }

  def +(scalar: Double): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorAddScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorAddScalar.launch(params, result)

    result
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](vectorRowSubMatrix, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    vectorRowSubMatrix.launch(params, result)

    result
  }

  def -|(matrix: Matrix[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](vectorColumnSubMatrix, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    vectorColumnSubMatrix.launch(params, result)

    result
  }

  def -(vector: Vector[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorSubVector, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorSubVector.launch(params, result)

    result
  }

  def -(scalar: Float): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorSubScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorSubScalar.launch(params, result)

    result
  }

  def -(scalar: Double): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorSubScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorSubScalar.launch(params, result)

    result
  }

  def *(scalar: Float): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorMulScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorMulScalar.launch(params, result)

    result
  }

  def *(scalar: Double): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorMulScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorMulScalar.launch(params, result)

    result
  }

  def *(vector: Vector[T]): Scalar[T] = {
    val result = resultsCache.result[T, Scalar[T]](vectorMulVector, vector, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorMulVector.launch(params, result)

    result
  }

  def *(matrix: Matrix[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorMulMatrix, matrix, new Vector[T](matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length)),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](result.length))
    )

    vectorMulMatrix.launch(params, result)

    result
  }

  def **(vector: Vector[T]): Matrix[T] = {
    val result = resultsCache.result[T, Matrix[T]](vectorMatrixMulVector, vector, new Matrix[T](length, vector.length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length)), Pointer.to(Array[Int](vector.length))
    )

    vectorMatrixMulVector.launch(params, result)

    result
  }

  def :*(vector: Vector[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorElementWiseMulVector, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorElementWiseMulVector.launch(params, result)

    result
  }

  def /(scalar: Float): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorDivScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorDivScalar.launch(params, result)

    result
  }

  def /(scalar: Double): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorDivScalar, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorDivScalar.launch(params, result)

    result
  }

  def :/(vector: Vector[T]): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorElementWiseDivVector, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorElementWiseDivVector.launch(params, result)

    result
  }

  def pow2(): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorPow2, 2, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorPow2.launch(params, result)

    result
  }

  def pow(power: Float): Vector[T] = {
    val result = resultsCache.result[T, Vector[T]](vectorPow, power, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](power)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorPow.launch(params, result)

    result
  }

  def ^(power: Float): Vector[T] = {
    if (power == 2) {
      pow2()
    } else {
      pow(power)
    }
  }

  def sum(): Scalar[T] = {
    val result = resultsCache.result[T, Scalar[T]](vectorSum, this, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    vectorSum.launch(params, result)

    result
  }

}

object Vector {

  lazy val vectorAddVector = new VectorAddVector
  lazy val vectorAddScalar = new VectorAddScalar
  lazy val vectorSubVector = new VectorSubVector
  lazy val vectorSubScalar = new VectorSubScalar
  lazy val vectorMulScalar = new VectorMulScalar
  lazy val vectorMulVector = new VectorMulVector
  lazy val vectorMulMatrix = new VectorMulMatrix
  lazy val vectorMatrixMulVector = new VectorMatrixMulVector
  lazy val vectorElementWiseDivVector = new VectorElementWiseDivVector
  lazy val vectorElementWiseMulVector = new VectorElementWiseMulVector
  lazy val vectorDivScalar = new VectorDivScalar
  lazy val vectorPow2 = new VectorPow2
  lazy val vectorPow = new VectorPow
  lazy val vectorExp = new VectorExp
  lazy val vectorSum = new VectorSum
  lazy val vectorRowAddMatrix = new VectorRowAddMatrix
  lazy val vectorColumnAddMatrix = new VectorColumnAddMatrix
  lazy val vectorRowSubMatrix = new VectorRowSubMatrix
  lazy val vectorColumnSubMatrix = new VectorColumnSubMatrix

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