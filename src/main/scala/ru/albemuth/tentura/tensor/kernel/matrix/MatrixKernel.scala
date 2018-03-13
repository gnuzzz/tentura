package ru.albemuth.tentura.tensor.kernel.matrix

import MatrixKernel.{BLOCK_ROWS, TILE_DIM}
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.{GenericKernel, KernelTemplate}
import ru.albemuth.tentura.tensor.{Matrix, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class MatrixKernel(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) {

  def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, TILE_DIM, 1)
//  def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, BLOCK_ROWS, 1)

  def gridSize(c: Matrix[_]): (Int, Int, Int) =
    ((c.columns - 1) / TILE_DIM + 1, (c.rows - 1) / TILE_DIM + 1, 1)

  def launch(params: Pointer, result: Matrix[_]): Unit = {
    val block = blockSize(result)
    val grid = gridSize(result)

    JCudaDriver.cuLaunchKernel(
      function,
      grid._1, grid._2, grid._3,
      block._1, block._2, block._3,
      //      TiledMatrixMultiplicationKernel.TILE_WIDTH * TiledMatrixMultiplicationKernel.TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }
}

object MatrixKernel {

  val TILE_DIM = 32
  val BLOCK_ROWS = 4

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, Unit, r)
    MatrixKernel.matrix_r(kernel, matrix, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Boolean, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Byte](if (scalar) 1 else 0)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Boolean, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Byte, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Byte, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Char, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Char](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Char, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Short, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Short, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Int, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Int, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Long, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Long, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Float, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Float, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Double, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T], scalar: Double, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, scalar, r)
    MatrixKernel.matrix_r(kernel, matrix, scalar, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Boolean, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Boolean, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Byte, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Byte, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Char, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Char, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Short, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Short, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Int, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Int, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Long, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Long, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Float, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Float, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Double, result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T], scalar: Double, r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T], matrix, scalar, r)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T1], vector: Vector[T2], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix: Matrix[T1], vector: Vector[T2], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, vector, r)
    MatrixKernel.matrix_r(kernel, matrix, vector, result)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T1], vector: Vector[T2], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T1], matrix, vector, result)
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix: Matrix[T1], vector: Vector[T2], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T1], matrix, vector, r)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, vector: Vector[T1], matrix: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, vector: Vector[T1], matrix: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    val result = vector.result(kernel, matrix, r)
    MatrixKernel.matrix_r(kernel, vector, matrix, result)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], vector: Vector[T1], matrix: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T1], vector, matrix, result)
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], vector: Vector[T1], matrix: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T1], vector, matrix, r)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, vector1: Vector[T1], vector2: Vector[T2], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(vector1.deviceDataPtr), Pointer.to(vector2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector1.length)), Pointer.to(Array[Int](vector2.length))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, vector1: Vector[T1], vector2: Vector[T2], r: => Matrix[R]): Matrix[R] = {
    val result = vector1.result(kernel, vector2, r)
    MatrixKernel.matrix_r(kernel, vector1, vector2, result)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], vector1: Vector[T1], vector2: Vector[T2], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T1], vector1, vector2, result)
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], vector1: Vector[T1], vector2: Vector[T2], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T1], vector1, vector2, r)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix1: Matrix[T1], matrix2: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix1.deviceDataPtr), Pointer.to(matrix2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix1.rows)), Pointer.to(Array[Int](matrix1.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix1: Matrix[T1], matrix2: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    val result = matrix1.result(kernel, matrix2, r)
    MatrixKernel.matrix_r(kernel, matrix1, matrix2, result)
  }

  def matrix_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix1: Matrix[T1], matrix2: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix_r(template.kernel[T1], matrix1, matrix2, result)
  }

  def matrix[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix1: Matrix[T1], matrix2: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix(template.kernel[T1], matrix1, matrix2, r)
  }

  def matrix2_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix1: Matrix[T1], matrix2: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix1.deviceDataPtr), Pointer.to(matrix2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix1.rows)), Pointer.to(Array[Int](matrix1.columns)),
      Pointer.to(Array[Int](matrix2.rows)), Pointer.to(Array[Int](matrix2.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix2[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: MatrixKernel, matrix1: Matrix[T1], matrix2: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    val result = matrix1.result(kernel, matrix2, r)
    MatrixKernel.matrix2_r(kernel, matrix1, matrix2, result)
  }

  def matrix2_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix1: Matrix[T1], matrix2: Matrix[T2], result: Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix2_r(template.kernel[T1], matrix1, matrix2, result)
  }

  def matrix2[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: MatrixKernel], matrix1: Matrix[T1], matrix2: Matrix[T2], r: => Matrix[R]): Matrix[R] = {
    MatrixKernel.matrix2(template.kernel[T1], matrix1, matrix2, r)
  }

}