package ru.albemuth.tentura.tensor

import jcuda.Pointer
import ru.albemuth.tentura.tensor.kernel._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object Functions {

  def sigmoid(scalar: Float): Float = {
    sigmoid(scalar.toDouble).toFloat
  }

  def sigmoid(scalar: Double): Double = {
    1.0 / (1.0 + Math.exp(scalar))
  }

  def sigmoid[T: ClassTag](scalar: Scalar[T]): Scalar[T] = {
    val result = scalar.result(scalarSigmoid, scalar, new Scalar[T]())

    val params = Pointer.to(
      Pointer.to(scalar.deviceDataPtr), Pointer.to(result.deviceDataPtr)
    )

    scalarSigmoid.launch(params, result)

    result
  }

  def sigmoid[T: ClassTag](vector: Vector[T]): Vector[T] = {
    val result = vector.result(vectorSigmoid, vector, new Vector[T](vector.length))

    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    vectorSigmoid.launch(params, result)

    result
  }

  def sigmoid[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    val result = matrix.result(matrixSigmoid, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    matrixSigmoid.launch(params, result)

    result
  }

  def exp[T: ClassTag](vector: Vector[T]): Vector[T] = {
    val result = vector.result(vectorExp, vector, new Vector[T](vector.length))

    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    vectorExp.launch(params, result)

    result
  }

  def exp[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    val result = matrix.result(matrixExp, matrix, new Matrix[T](matrix.rows, matrix.columns))

    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    matrixExp.launch(params, result)

    result
  }

  lazy val vectorExp = new VectorExp
  lazy val matrixExp = new MatrixExp
  lazy val scalarSigmoid = new ScalarSigmoid
  lazy val vectorSigmoid = new VectorSigmoid
  lazy val matrixSigmoid = new MatrixSigmoid
  
}
