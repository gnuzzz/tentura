package ru.albemuth.tentura.tensor

import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{array, devicePtr, pointer}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.{ScalarDivMatrix, ScalarSubMatrix}
import ru.albemuth.tentura.tensor.kernel.vector.{ScalarDivVector, ScalarSubVector}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Scalar[T: ClassTag]() extends DeviceVar[T] {

  override protected[tensor] val data: Array[T] = array[T](1)
  override protected[tensor] val dataPointer: Pointer = pointer(data)
  override protected[tensor] val deviceDataPtr: CUdeviceptr = devicePtr(data)

  def this(scalar: T) {
    this()
    data(0) = scalar
    copy2device()
  }

  def value(): T = data(0)

  protected[tensor] def result(kernel: ScalarKernel, resultKey: Any, result: => Scalar[T]): Scalar[T] = {
    resultsCache.result[T, Scalar[T]](kernel, resultKey, result)
  }

}

object Scalar {

  lazy val scalarSubVector = new KernelTemplate(new ScalarSubVector)
  lazy val scalarSubMatrix = new KernelTemplate(new ScalarSubMatrix)
  lazy val scalarDivVector = new KernelTemplate(new ScalarDivVector)
  lazy val scalarDivMatrix = new KernelTemplate(new ScalarDivMatrix)

  def apply[T: ClassTag](scalar: T): Scalar[T] = new Scalar(scalar)

  implicit def toBoolean(scalar: Scalar[Boolean]): Boolean = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toByte(scalar: Scalar[Byte]): Byte = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toShort(scalar: Scalar[Short]): Short = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toInt(scalar: Scalar[Int]): Float = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toLong(scalar: Scalar[Long]): Float = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toFloat(scalar: Scalar[Float]): Float = {
    scalar.copy2host()
    scalar.value()
  }

  implicit def toDouble(scalar: Scalar[Double]): Double = {
    scalar.copy2host()
    scalar.value()
  }

  implicit class ScalarByte(scalar: Byte) {

    def +(vector: Vector[Byte]): Vector[Byte] = {
      vector + scalar
    }

    def +(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix + scalar
    }

    def -(vector: Vector[Byte]): Vector[Byte] = {
      val kernel = scalarSubVector.kernel[Byte]
      val result = vector.result(kernel, scalar, new Vector[Byte](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Byte](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Byte]): Matrix[Byte] = {
      val kernel = scalarSubMatrix.kernel[Byte]
      val result = matrix.result(kernel, scalar, new Matrix[Byte](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Byte](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Byte]): Vector[Byte] = {
      vector * scalar
    }

    def *(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix * scalar
    }

    def /(vector: Vector[Byte]): Vector[Byte] = {
      val kernel = scalarDivVector.kernel[Byte]
      val result = vector.result(kernel, scalar, new Vector[Byte](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Byte](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Byte]): Matrix[Byte] = {
      val kernel = scalarDivMatrix.kernel[Byte]
      val result = matrix.result(kernel, scalar, new Matrix[Byte](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Byte](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarShort(scalar: Short) {

    def +(vector: Vector[Short]): Vector[Short] = {
      vector + scalar
    }

    def +(matrix: Matrix[Short]): Matrix[Short] = {
      matrix + scalar
    }

    def -(vector: Vector[Short]): Vector[Short] = {
      val kernel = scalarSubVector.kernel[Short]
      val result = vector.result(kernel, scalar, new Vector[Short](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Short](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Short]): Matrix[Short] = {
      val kernel = scalarSubMatrix.kernel[Short]
      val result = matrix.result(kernel, scalar, new Matrix[Short](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Short](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Short]): Vector[Short] = {
      vector * scalar
    }

    def *(matrix: Matrix[Short]): Matrix[Short] = {
      matrix * scalar
    }

    def /(vector: Vector[Short]): Vector[Short] = {
      val kernel = scalarDivVector.kernel[Short]
      val result = vector.result(kernel, scalar, new Vector[Short](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Short](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Short]): Matrix[Short] = {
      val kernel = scalarDivMatrix.kernel[Short]
      val result = matrix.result(kernel, scalar, new Matrix[Short](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Short](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarChar(scalar: Char) {

    def +(vector: Vector[Char]): Vector[Char] = {
      vector + scalar
    }

    def +(matrix: Matrix[Char]): Matrix[Char] = {
      matrix + scalar
    }

    def -(vector: Vector[Char]): Vector[Char] = {
      val kernel = scalarSubVector.kernel[Char]
      val result = vector.result(kernel, scalar, new Vector[Char](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Char](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Char]): Matrix[Char] = {
      val kernel = scalarSubMatrix.kernel[Char]
      val result = matrix.result(kernel, scalar, new Matrix[Char](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Char](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Char]): Vector[Char] = {
      vector * scalar
    }

    def *(matrix: Matrix[Char]): Matrix[Char] = {
      matrix * scalar
    }

    def /(vector: Vector[Char]): Vector[Char] = {
      val kernel = scalarDivVector.kernel[Char]
      val result = vector.result(kernel, scalar, new Vector[Char](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Char](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Char]): Matrix[Char] = {
      val kernel = scalarDivMatrix.kernel[Char]
      val result = matrix.result(kernel, scalar, new Matrix[Char](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Char](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarInt(scalar: Int) {

    def +(vector: Vector[Int]): Vector[Int] = {
      vector + scalar
    }

    def +(matrix: Matrix[Int]): Matrix[Int] = {
      matrix + scalar
    }

    def -(vector: Vector[Int]): Vector[Int] = {
      val kernel = scalarSubVector.kernel[Int]
      val result = vector.result(kernel, scalar, new Vector[Int](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Int](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Int]): Matrix[Int] = {
      val kernel = scalarSubMatrix.kernel[Int]
      val result = matrix.result(kernel, scalar, new Matrix[Int](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Int](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Int]): Vector[Int] = {
      vector * scalar
    }

    def *(matrix: Matrix[Int]): Matrix[Int] = {
      matrix * scalar
    }

    def /(vector: Vector[Int]): Vector[Int] = {
      val kernel = scalarDivVector.kernel[Int]
      val result = vector.result(kernel, scalar, new Vector[Int](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Int](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Int]): Matrix[Int] = {
      val kernel = scalarDivMatrix.kernel[Int]
      val result = matrix.result(kernel, scalar, new Matrix[Int](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Int](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarLong(scalar: Long) {

    def +(vector: Vector[Long]): Vector[Long] = {
      vector + scalar
    }

    def +(matrix: Matrix[Long]): Matrix[Long] = {
      matrix + scalar
    }

    def -(vector: Vector[Long]): Vector[Long] = {
      val kernel = scalarSubVector.kernel[Long]
      val result = vector.result(kernel, scalar, new Vector[Long](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Long](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Long]): Matrix[Long] = {
      val kernel = scalarSubMatrix.kernel[Long]
      val result = matrix.result(kernel, scalar, new Matrix[Long](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Long](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Long]): Vector[Long] = {
      vector * scalar
    }

    def *(matrix: Matrix[Long]): Matrix[Long] = {
      matrix * scalar
    }

    def /(vector: Vector[Long]): Vector[Long] = {
      val kernel = scalarDivVector.kernel[Long]
      val result = vector.result(kernel, scalar, new Vector[Long](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Long](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Long]): Matrix[Long] = {
      val kernel = scalarDivMatrix.kernel[Long]
      val result = matrix.result(kernel, scalar, new Matrix[Long](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Long](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarFloat(scalar: Float) {

    def +(vector: Vector[Float]): Vector[Float] = {
      vector + scalar
    }

    def +(matrix: Matrix[Float]): Matrix[Float] = {
      matrix + scalar
    }

    def -(vector: Vector[Float]): Vector[Float] = {
      val kernel = scalarSubVector.kernel[Float]
      val result = vector.result(kernel, scalar, new Vector[Float](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Float]): Matrix[Float] = {
      val kernel = scalarSubMatrix.kernel[Float]
      val result = matrix.result(kernel, scalar, new Matrix[Float](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Float]): Vector[Float] = {
      vector * scalar
    }

    def *(matrix: Matrix[Float]): Matrix[Float] = {
      matrix * scalar
    }

    def /(vector: Vector[Float]): Vector[Float] = {
      val kernel = scalarDivVector.kernel[Float]
      val result = vector.result(kernel, scalar, new Vector[Float](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Float]): Matrix[Float] = {
      val kernel = scalarDivMatrix.kernel[Float]
      val result = matrix.result(kernel, scalar, new Matrix[Float](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

  implicit class ScalarDouble(scalar: Double) {

    def +(vector: Vector[Double]): Vector[Double] = {
      vector + scalar
    }

    def +(matrix: Matrix[Double]): Matrix[Double] = {
      matrix + scalar
    }

    def -(vector: Vector[Double]): Vector[Double] = {
      val kernel = scalarSubVector.kernel[Double]
      val result = vector.result(kernel, scalar, new Vector[Double](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Double](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def -(matrix: Matrix[Double]): Matrix[Double] = {
      val kernel = scalarSubMatrix.kernel[Double]
      val result = matrix.result(kernel, scalar, new Matrix[Double](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Double](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

    def *(vector: Vector[Double]): Vector[Double] = {
      vector * scalar
    }

    def *(matrix: Matrix[Double]): Matrix[Double] = {
      matrix * scalar
    }

    def /(vector: Vector[Double]): Vector[Double] = {
      val kernel = scalarDivVector.kernel[Double]
      val result = vector.result(kernel, scalar, new Vector[Double](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Double](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      kernel.launch(params, result)

      result
    }

    def /(matrix: Matrix[Double]): Matrix[Double] = {
      val kernel = scalarDivMatrix.kernel[Double]
      val result = matrix.result(kernel, scalar, new Matrix[Double](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Double](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      kernel.launch(params, result)

      result
    }

  }

}
