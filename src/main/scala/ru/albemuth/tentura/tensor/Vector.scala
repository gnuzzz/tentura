package ru.albemuth.tentura.tensor

import Vector._
import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, pointer}
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelTemplate}
import ru.albemuth.tentura.tensor.MathFunctions.{pow, pow2, powd, pow2d}
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

  def apply(i: Int): T = data(i)

  def values(): Array[T] = {
    val ret = new Array[T](length)
    System.arraycopy(data, 0, ret, 0, length)
    ret
  }

  protected[tensor] def result[R: ClassTag](kernel: JCudaKernel, resultKey: Any, result: => Vector[R]): Vector[R] = {
    resultsCache.result[R, Vector[R]](kernel, resultKey, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    val kernel = vectorRowAddMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def +|(matrix: Matrix[T]): Matrix[T] = {
    val kernel = vectorColumnAddMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def +(vector: Vector[T]): Vector[T] = {
    val kernel = vectorAddVector.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Byte): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Short): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Int): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Long): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Float): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def +(scalar: Double): Vector[T] = {
    val kernel = vectorAddScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    val kernel = vectorRowSubMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def -|(matrix: Matrix[T]): Matrix[T] = {
    val kernel = vectorColumnSubMatrix.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def -(vector: Vector[T]): Vector[T] = {
    val kernel = vectorSubVector.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Byte): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Short): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Int): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Long): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Float): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def -(scalar: Double): Vector[T] = {
    val kernel = vectorSubScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Byte): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Short): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Int): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Long): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Float): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(scalar: Double): Vector[T] = {
    val kernel = vectorMulScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(vector: Vector[T]): Scalar[T] = {
    val kernel = vectorMulVector.kernel[T]
    val result = resultsCache.result[T, Scalar[T]](kernel, vector, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def *(matrix: Matrix[T]): Vector[T] = {
    val kernel = vectorMulMatrix.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, matrix, new Vector[T](matrix.columns))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length)),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def **(vector: Vector[T]): Matrix[T] = {
    val kernel = vectorMatrixMulVector.kernel[T]
    val result = resultsCache.result[T, Matrix[T]](kernel, vector, new Matrix[T](length, vector.length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length)), Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def :*(vector: Vector[T]): Vector[T] = {
    val kernel = vectorElementWiseMulVector.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Byte): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Short): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Int): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Long): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Float): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def /(scalar: Double): Vector[T] = {
    val kernel = vectorDivScalar.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, scalar, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
  }

  def :/(vector: Vector[T]): Vector[T] = {
    val kernel = vectorElementWiseDivVector.kernel[T]
    val result = resultsCache.result[T, Vector[T]](kernel, vector, new Vector[T](length))

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
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
    val kernel = vectorSum.kernel[T]
    val result = resultsCache.result[T, Scalar[T]](kernel, this, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](length))
    )

    kernel.launch(params, result)

    result
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