package ru.albemuth.tentura.tensor

import jcuda.Pointer
import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{array, devicePtr, pointer}
import ru.albemuth.tentura.tensor.kernel.{ScalarDivMatrix, ScalarDivVector, ScalarKernel, ScalarSubMatrix, ScalarSubVector}

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

  lazy val scalarSubVector = new ScalarSubVector
  lazy val scalarSubMatrix = new ScalarSubMatrix
  lazy val scalarDivVector = new ScalarDivVector
  lazy val scalarDivMatrix = new ScalarDivMatrix

  def apply[T: ClassTag](scalar: T): Scalar[T] = new Scalar(scalar)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Byte](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Byte]): Matrix[Byte] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Byte](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Byte]): Vector[Byte] = {
      vector * scalar
    }

    def *(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix * scalar
    }

    def /(vector: Vector[Byte]): Vector[Byte] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Byte](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Byte]): Matrix[Byte] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Byte](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Short](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Short]): Matrix[Short] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Short](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Short]): Vector[Short] = {
      vector * scalar
    }

    def *(matrix: Matrix[Short]): Matrix[Short] = {
      matrix * scalar
    }

    def /(vector: Vector[Short]): Vector[Short] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Short](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Short]): Matrix[Short] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Short](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Char](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Char]): Matrix[Char] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Char](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Char]): Vector[Char] = {
      vector * scalar
    }

    def *(matrix: Matrix[Char]): Matrix[Char] = {
      matrix * scalar
    }

    def /(vector: Vector[Char]): Vector[Char] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Char](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Char]): Matrix[Char] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Char](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Int](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Int]): Matrix[Int] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Int](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Int]): Vector[Int] = {
      vector * scalar
    }

    def *(matrix: Matrix[Int]): Matrix[Int] = {
      matrix * scalar
    }

    def /(vector: Vector[Int]): Vector[Int] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Int](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Int]): Matrix[Int] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Int](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Long](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Long]): Matrix[Long] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Long](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Long]): Vector[Long] = {
      vector * scalar
    }

    def *(matrix: Matrix[Long]): Matrix[Long] = {
      matrix * scalar
    }

    def /(vector: Vector[Long]): Vector[Long] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Long](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Long]): Matrix[Long] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Long](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Float](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Float]): Matrix[Float] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Float](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Float]): Vector[Float] = {
      vector * scalar
    }

    def *(matrix: Matrix[Float]): Matrix[Float] = {
      matrix * scalar
    }

    def /(vector: Vector[Float]): Vector[Float] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Float](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Float]): Matrix[Float] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Float](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

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
      val result = vector.result(scalarSubVector, scalar, new Vector[Double](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarSubVector.launch(params, result)

      result
    }

    def -(matrix: Matrix[Double]): Matrix[Double] = {
      val result = matrix.result(scalarSubMatrix, scalar, new Matrix[Double](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarSubMatrix.launch(params, result)

      result
    }

    def *(vector: Vector[Double]): Vector[Double] = {
      vector * scalar
    }

    def *(matrix: Matrix[Double]): Matrix[Double] = {
      matrix * scalar
    }

    def /(vector: Vector[Double]): Vector[Double] = {
      val result = vector.result(scalarDivVector, scalar, new Vector[Double](vector.length))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](result.length))
      )

      scalarDivVector.launch(params, result)

      result
    }

    def /(matrix: Matrix[Double]): Matrix[Double] = {
      val result = matrix.result(scalarDivMatrix, scalar, new Matrix[Double](matrix.rows, matrix.columns))

      val params = Pointer.to(
        Pointer.to(Array[Float](scalar.toFloat)), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
        Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
      )

      scalarDivMatrix.launch(params, result)

      result
    }

  }

}
