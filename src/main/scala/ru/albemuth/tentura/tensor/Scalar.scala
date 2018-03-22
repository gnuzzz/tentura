package ru.albemuth.tentura.tensor

import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.devicePtr
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.{MatrixKernel, ScalarDivMatrix, ScalarSubMatrix}
import ru.albemuth.tentura.tensor.kernel.vector.{ScalarDivVector, ScalarSubVector, VectorKernel}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Scalar[T: ClassTag](override val deviceDataPtr: CUdeviceptr) extends DeviceVar[T] {

  def this() {
    this(devicePtr(1))
  }

  def this(scalar: T) {
    this(devicePtr(1))
    copy2device(Array(scalar))
  }

  def value(): T = {
    val data = Array.ofDim[T](1)
    copy2host(data)
    data(0)
  }

}

object Scalar {

  lazy val scalarSubVector = new KernelTemplate(new ScalarSubVector)
  lazy val scalarSubMatrix = new KernelTemplate(new ScalarSubMatrix)
  lazy val scalarDivVector = new KernelTemplate(new ScalarDivVector)
  lazy val scalarDivMatrix = new KernelTemplate(new ScalarDivMatrix)

  def apply[T: ClassTag](scalar: T): Scalar[T] = new Scalar(scalar)

  implicit def toBoolean(scalar: Scalar[Boolean]): Boolean = {
    scalar.value()
  }

  implicit def toByte(scalar: Scalar[Byte]): Byte = {
    scalar.value()
  }

  implicit def toShort(scalar: Scalar[Short]): Short = {
    scalar.value()
  }

  implicit def toInt(scalar: Scalar[Int]): Float = {
    scalar.value()
  }

  implicit def toLong(scalar: Scalar[Long]): Float = {
    scalar.value()
  }

  implicit def toFloat(scalar: Scalar[Float]): Float = {
    scalar.value()
  }

  implicit def toDouble(scalar: Scalar[Double]): Double = {
    scalar.value()
  }

  implicit class ScalarByte(scalar: Byte) {

    def +(vector: Vector[Byte]): Vector[Byte] = {
      vector + scalar
    }

    def +(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix + scalar
    }

    def -(vector: Vector[Byte], result: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Byte](vector.length))
    }

    def -(matrix: Matrix[Byte], result: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Byte](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Byte]): Vector[Byte] = {
      vector * scalar
    }

    def *(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix * scalar
    }

    def /(vector: Vector[Byte], result: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Byte](vector.length))
    }

    def /(matrix: Matrix[Byte], result: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Byte](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarShort(scalar: Short) {

    def +(vector: Vector[Short]): Vector[Short] = {
      vector + scalar
    }

    def +(matrix: Matrix[Short]): Matrix[Short] = {
      matrix + scalar
    }

    def -(vector: Vector[Short], result: Vector[Short]): Vector[Short] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Short]): Vector[Short] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Short](vector.length))
    }

    def -(matrix: Matrix[Short], result: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Short](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Short]): Vector[Short] = {
      vector * scalar
    }

    def *(matrix: Matrix[Short]): Matrix[Short] = {
      matrix * scalar
    }

    def /(vector: Vector[Short], result: Vector[Short]): Vector[Short] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Short]): Vector[Short] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Short](vector.length))
    }

    def /(matrix: Matrix[Short], result: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Short](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarChar(scalar: Char) {

    def +(vector: Vector[Char]): Vector[Char] = {
      vector + scalar
    }

    def +(matrix: Matrix[Char]): Matrix[Char] = {
      matrix + scalar
    }

    def -(vector: Vector[Char], result: Vector[Char]): Vector[Char] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Char]): Vector[Char] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Char](vector.length))
    }

    def -(matrix: Matrix[Char], result: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Char](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Char]): Vector[Char] = {
      vector * scalar
    }

    def *(matrix: Matrix[Char]): Matrix[Char] = {
      matrix * scalar
    }

    def /(vector: Vector[Char], result: Vector[Char]): Vector[Char] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Char]): Vector[Char] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Char](vector.length))
    }

    def /(matrix: Matrix[Char], result: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Char](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarInt(scalar: Int) {

    def +(vector: Vector[Int]): Vector[Int] = {
      vector + scalar
    }

    def +(matrix: Matrix[Int]): Matrix[Int] = {
      matrix + scalar
    }

    def -(vector: Vector[Int], result: Vector[Int]): Vector[Int] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Int]): Vector[Int] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Int](vector.length))
    }

    def -(matrix: Matrix[Int], result: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Int](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Int]): Vector[Int] = {
      vector * scalar
    }

    def *(matrix: Matrix[Int]): Matrix[Int] = {
      matrix * scalar
    }

    def /(vector: Vector[Int], result: Vector[Int]): Vector[Int] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Int]): Vector[Int] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Int](vector.length))
    }

    def /(matrix: Matrix[Int], result: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Int](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarLong(scalar: Long) {

    def +(vector: Vector[Long]): Vector[Long] = {
      vector + scalar
    }

    def +(matrix: Matrix[Long]): Matrix[Long] = {
      matrix + scalar
    }

    def -(vector: Vector[Long], result: Vector[Long]): Vector[Long] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Long]): Vector[Long] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Long](vector.length))
    }

    def -(matrix: Matrix[Long], result: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Long](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Long]): Vector[Long] = {
      vector * scalar
    }

    def *(matrix: Matrix[Long]): Matrix[Long] = {
      matrix * scalar
    }

    def /(vector: Vector[Long], result: Vector[Long]): Vector[Long] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Long]): Vector[Long] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Long](vector.length))
    }

    def /(matrix: Matrix[Long], result: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Long](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarFloat(scalar: Float) {

    def +(vector: Vector[Float]): Vector[Float] = {
      vector + scalar
    }

    def +(matrix: Matrix[Float]): Matrix[Float] = {
      matrix + scalar
    }

    def -(vector: Vector[Float], result: Vector[Float]): Vector[Float] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Float]): Vector[Float] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Float](vector.length))
    }

    def -(matrix: Matrix[Float], result: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Float](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Float]): Vector[Float] = {
      vector * scalar
    }

    def *(matrix: Matrix[Float]): Matrix[Float] = {
      matrix * scalar
    }

    def /(vector: Vector[Float], result: Vector[Float]): Vector[Float] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Float]): Vector[Float] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Float](vector.length))
    }

    def /(matrix: Matrix[Float], result: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Float](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarDouble(scalar: Double) {

    def +(vector: Vector[Double]): Vector[Double] = {
      vector + scalar
    }

    def +(matrix: Matrix[Double]): Matrix[Double] = {
      matrix + scalar
    }

    def -(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
      VectorKernel.vector_r(scalarSubVector, vector, scalar, result)
    }

    def -(vector: Vector[Double]): Vector[Double] = {
      VectorKernel.vector(scalarSubVector, vector, scalar, new Vector[Double](vector.length))
    }

    def -(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix_r(scalarSubMatrix, matrix, scalar, result)
    }

    def -(matrix: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix(scalarSubMatrix, matrix, scalar, new Matrix[Double](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Double]): Vector[Double] = {
      vector * scalar
    }

    def *(matrix: Matrix[Double]): Matrix[Double] = {
      matrix * scalar
    }

    def /(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
      VectorKernel.vector_r(scalarDivVector, vector, scalar, result)
    }

    def /(vector: Vector[Double]): Vector[Double] = {
      VectorKernel.vector(scalarDivVector, vector, scalar, new Vector[Double](vector.length))
    }

    def /(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, scalar, result)
    }

    def /(matrix: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, scalar, new Matrix[Double](matrix.rows, matrix.columns))
    }

  }

}
