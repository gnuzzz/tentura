package ru.albemuth.tentura.tensor

import jcuda.driver.CUdeviceptr
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.devicePtr
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.{Matrix2ScalarElementwise, MatrixKernel}
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

  def +(operand: T): T = {
    val v = value()
    operand match {
      case b: Boolean => throw new IllegalArgumentException("Operator \"+\" not aplicable to boolean argumants")
      case b: Byte => (v.asInstanceOf[Byte] + b).asInstanceOf[T]
      case c: Char => (v.asInstanceOf[Char] + c).asInstanceOf[T]
      case s: Short => (v.asInstanceOf[Short] + s).asInstanceOf[T]
      case i: Int => (v.asInstanceOf[Int] + i).asInstanceOf[T]
      case l: Long => (v.asInstanceOf[Long] + l).asInstanceOf[T]
      case f: Float => (v.asInstanceOf[Float] + f).asInstanceOf[T]
      case d: Double => (v.asInstanceOf[Double] + d).asInstanceOf[T]
      case _ => throw new IllegalArgumentException(s"Unexpected operand type: ${operand.getClass.getName}")
    }
  }

  def -(operand: T): T = {
    val v = value()
    operand match {
      case b: Boolean => throw new IllegalArgumentException("Operator \"-\" not aplicable to boolean argumants")
      case b: Byte => (v.asInstanceOf[Byte] - b).asInstanceOf[T]
      case c: Char => (v.asInstanceOf[Char] - c).asInstanceOf[T]
      case s: Short => (v.asInstanceOf[Short] - s).asInstanceOf[T]
      case i: Int => (v.asInstanceOf[Int] - i).asInstanceOf[T]
      case l: Long => (v.asInstanceOf[Long] - l).asInstanceOf[T]
      case f: Float => (v.asInstanceOf[Float] - f).asInstanceOf[T]
      case d: Double => (v.asInstanceOf[Double] - d).asInstanceOf[T]
      case _ => throw new IllegalArgumentException(s"Unexpected operand type: ${operand.getClass.getName}")
    }
  }

  def *(operand: T): T = {
    val v = value()
    operand match {
      case b: Boolean => throw new IllegalArgumentException("Operator \"*\" not aplicable to boolean argumants")
      case b: Byte => (v.asInstanceOf[Byte] * b).asInstanceOf[T]
      case c: Char => (v.asInstanceOf[Char] * c).asInstanceOf[T]
      case s: Short => (v.asInstanceOf[Short] * s).asInstanceOf[T]
      case i: Int => (v.asInstanceOf[Int] * i).asInstanceOf[T]
      case l: Long => (v.asInstanceOf[Long] * l).asInstanceOf[T]
      case f: Float => (v.asInstanceOf[Float] * f).asInstanceOf[T]
      case d: Double => (v.asInstanceOf[Double] * d).asInstanceOf[T]
      case _ => throw new IllegalArgumentException(s"Unexpected operand type: ${operand.getClass.getName}")
    }
  }

  def /(operand: T): T = {
    val v = value()
    operand match {
      case b: Boolean => throw new IllegalArgumentException("Operator \"/\" not aplicable to boolean argumants")
      case b: Byte => (v.asInstanceOf[Byte] / b).asInstanceOf[T]
      case c: Char => (v.asInstanceOf[Char] / c).asInstanceOf[T]
      case s: Short => (v.asInstanceOf[Short] / s).asInstanceOf[T]
      case i: Int => (v.asInstanceOf[Int] / i).asInstanceOf[T]
      case l: Long => (v.asInstanceOf[Long] / l).asInstanceOf[T]
      case f: Float => (v.asInstanceOf[Float] / f).asInstanceOf[T]
      case d: Double => (v.asInstanceOf[Double] / d).asInstanceOf[T]
      case _ => throw new IllegalArgumentException(s"Unexpected operand type: ${operand.getClass.getName}")
    }
  }

}

object Scalar {

  private lazy val scalarSubVector = new KernelTemplate(new ScalarSubVector)
  private lazy val scalarMinusMatrix = new KernelTemplate(new  Matrix2ScalarElementwise("scalarMinusMatrix"))
  private lazy val scalarDivVector = new KernelTemplate(new ScalarDivVector)
  private lazy val scalarDivMatrix = new KernelTemplate(new  Matrix2ScalarElementwise("scalarDivMatrix"))

  def apply[T: ClassTag](scalar: T): Scalar[T] = new Scalar(scalar)

//  implicit def toBoolean(scalar: Scalar[Boolean]): Boolean = {
//    scalar.value()
//  }
//
//  implicit def toByte(scalar: Scalar[Byte]): Byte = {
//    scalar.value()
//  }
//
//  implicit def toShort(scalar: Scalar[Short]): Short = {
//    scalar.value()
//  }
//
//  implicit def toInt(scalar: Scalar[Int]): Float = {
//    scalar.value()
//  }
//
//  implicit def toLong(scalar: Scalar[Long]): Float = {
//    scalar.value()
//  }
//
//  implicit def toFloat(scalar: Scalar[Float]): Float = {
//    scalar.value()
//  }
//
//  implicit def toDouble(scalar: Scalar[Double]): Double = {
//    scalar.value()
//  }

  implicit class ScalarByte(value: Byte) {

    def +(scalar: Scalar[Byte]): Byte = {
      scalar + value
    }

    def -(scalar: Scalar[Byte]): Byte = {
      scalar - value
    }

    def *(scalar: Scalar[Byte]): Byte = {
      scalar * value
    }

    def /(scalar: Scalar[Byte]): Byte = {
      scalar / value
    }

    def +(vector: Vector[Byte]): Vector[Byte] = {
      vector + value
    }

    def +(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix + value
    }

    def -(vector: Vector[Byte], result: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Byte](vector.length))
    }

    def -(matrix: Matrix[Byte], result: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Byte](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Byte]): Vector[Byte] = {
      vector * value
    }

    def *(matrix: Matrix[Byte]): Matrix[Byte] = {
      matrix * value
    }

    def /(vector: Vector[Byte], result: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Byte]): Vector[Byte] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Byte](vector.length))
    }

    def /(matrix: Matrix[Byte], result: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Byte]): Matrix[Byte] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Byte](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarShort(value: Short) {

    def +(scalar: Scalar[Short]): Short = {
      (scalar + value).toShort
    }

    def -(scalar: Scalar[Short]): Short = {
      (scalar - value).toShort
    }

    def *(scalar: Scalar[Short]): Short = {
      (scalar * value).toShort
    }

    def /(scalar: Scalar[Short]): Short = {
      (scalar / value).toShort
    }

    def +(vector: Vector[Short]): Vector[Short] = {
      vector + value
    }

    def +(matrix: Matrix[Short]): Matrix[Short] = {
      matrix + value
    }

    def -(vector: Vector[Short], result: Vector[Short]): Vector[Short] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Short]): Vector[Short] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Short](vector.length))
    }

    def -(matrix: Matrix[Short], result: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Short](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Short]): Vector[Short] = {
      vector * value
    }

    def *(matrix: Matrix[Short]): Matrix[Short] = {
      matrix * value
    }

    def /(vector: Vector[Short], result: Vector[Short]): Vector[Short] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Short]): Vector[Short] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Short](vector.length))
    }

    def /(matrix: Matrix[Short], result: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Short]): Matrix[Short] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Short](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarChar(value: Char) {

    def +(scalar: Scalar[Char]): Char = {
      (scalar + value).toChar
    }

    def -(scalar: Scalar[Char]): Char = {
      (scalar - value).toChar
    }

    def *(scalar: Scalar[Char]): Char = {
      (scalar * value).toChar
    }

    def /(scalar: Scalar[Char]): Char = {
      (scalar / value).toChar
    }

    def +(vector: Vector[Char]): Vector[Char] = {
      vector + value
    }

    def +(matrix: Matrix[Char]): Matrix[Char] = {
      matrix + value
    }

    def -(vector: Vector[Char], result: Vector[Char]): Vector[Char] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Char]): Vector[Char] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Char](vector.length))
    }

    def -(matrix: Matrix[Char], result: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Char](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Char]): Vector[Char] = {
      vector * value
    }

    def *(matrix: Matrix[Char]): Matrix[Char] = {
      matrix * value
    }

    def /(vector: Vector[Char], result: Vector[Char]): Vector[Char] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Char]): Vector[Char] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Char](vector.length))
    }

    def /(matrix: Matrix[Char], result: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Char]): Matrix[Char] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Char](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarInt(value: Int) {

    def +(scalar: Scalar[Int]): Int = {
      scalar + value
    }

    def -(scalar: Scalar[Int]): Int = {
      scalar - value
    }

    def *(scalar: Scalar[Int]): Int = {
      scalar * value
    }

    def /(scalar: Scalar[Int]): Int = {
      scalar / value
    }

    def +(vector: Vector[Int]): Vector[Int] = {
      vector + value
    }

    def +(matrix: Matrix[Int]): Matrix[Int] = {
      matrix + value
    }

    def -(vector: Vector[Int], result: Vector[Int]): Vector[Int] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Int]): Vector[Int] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Int](vector.length))
    }

    def -(matrix: Matrix[Int], result: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Int](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Int]): Vector[Int] = {
      vector * value
    }

    def *(matrix: Matrix[Int]): Matrix[Int] = {
      matrix * value
    }

    def /(vector: Vector[Int], result: Vector[Int]): Vector[Int] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Int]): Vector[Int] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Int](vector.length))
    }

    def /(matrix: Matrix[Int], result: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Int]): Matrix[Int] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Int](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarLong(value: Long) {

    def +(scalar: Scalar[Long]): Long = {
      scalar + value
    }

    def -(scalar: Scalar[Long]): Long = {
      scalar - value
    }

    def *(scalar: Scalar[Long]): Long = {
      scalar * value
    }

    def /(scalar: Scalar[Long]): Long = {
      scalar / value
    }

    def +(vector: Vector[Long]): Vector[Long] = {
      vector + value
    }

    def +(matrix: Matrix[Long]): Matrix[Long] = {
      matrix + value
    }

    def -(vector: Vector[Long], result: Vector[Long]): Vector[Long] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Long]): Vector[Long] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Long](vector.length))
    }

    def -(matrix: Matrix[Long], result: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Long](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Long]): Vector[Long] = {
      vector * value
    }

    def *(matrix: Matrix[Long]): Matrix[Long] = {
      matrix * value
    }

    def /(vector: Vector[Long], result: Vector[Long]): Vector[Long] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Long]): Vector[Long] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Long](vector.length))
    }

    def /(matrix: Matrix[Long], result: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Long]): Matrix[Long] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Long](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarFloat(value: Float) {

    def +(scalar: Scalar[Float]): Float = {
      scalar + value
    }

    def -(scalar: Scalar[Float]): Float = {
      scalar - value
    }

    def *(scalar: Scalar[Float]): Float = {
      scalar * value
    }

    def /(scalar: Scalar[Float]): Float = {
      scalar / value
    }

    def +(vector: Vector[Float]): Vector[Float] = {
      vector + value
    }

    def +(matrix: Matrix[Float]): Matrix[Float] = {
      matrix + value
    }

    def -(vector: Vector[Float], result: Vector[Float]): Vector[Float] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Float]): Vector[Float] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Float](vector.length))
    }

    def -(matrix: Matrix[Float], result: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Float](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Float]): Vector[Float] = {
      vector * value
    }

    def *(matrix: Matrix[Float]): Matrix[Float] = {
      matrix * value
    }

    def /(vector: Vector[Float], result: Vector[Float]): Vector[Float] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Float]): Vector[Float] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Float](vector.length))
    }

    def /(matrix: Matrix[Float], result: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Float]): Matrix[Float] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Float](matrix.rows, matrix.columns))
    }

  }

  implicit class ScalarDouble(value: Double) {

    def +(scalar: Scalar[Double]): Double = {
      scalar + value
    }

    def -(scalar: Scalar[Double]): Double = {
      scalar - value
    }

    def *(scalar: Scalar[Double]): Double = {
      scalar * value
    }

    def /(scalar: Scalar[Double]): Double = {
      scalar / value
    }

    def +(vector: Vector[Double]): Vector[Double] = {
      vector + value
    }

    def +(matrix: Matrix[Double]): Matrix[Double] = {
      matrix + value
    }

    def -(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
      VectorKernel.vector_r(scalarSubVector, vector, value, result)
    }

    def -(vector: Vector[Double]): Vector[Double] = {
      VectorKernel.vector(scalarSubVector, vector, value, new Vector[Double](vector.length))
    }

    def -(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix_r(scalarMinusMatrix, matrix, value, result)
    }

    def -(matrix: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix(scalarMinusMatrix, matrix, value, new Matrix[Double](matrix.rows, matrix.columns))
    }

    def *(vector: Vector[Double]): Vector[Double] = {
      vector * value
    }

    def *(matrix: Matrix[Double]): Matrix[Double] = {
      matrix * value
    }

    def /(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
      VectorKernel.vector_r(scalarDivVector, vector, value, result)
    }

    def /(vector: Vector[Double]): Vector[Double] = {
      VectorKernel.vector(scalarDivVector, vector, value, new Vector[Double](vector.length))
    }

    def /(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix_r(scalarDivMatrix, matrix, value, result)
    }

    def /(matrix: Matrix[Double]): Matrix[Double] = {
      MatrixKernel.matrix(scalarDivMatrix, matrix, value, new Matrix[Double](matrix.rows, matrix.columns))
    }

  }

}
