package ru.albemuth.tentura.tensor.kernel.vector

import VectorKernel.TILE_DIM
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.{GenericKernel, KernelTemplate}
import ru.albemuth.tentura.tensor.{Matrix, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class VectorKernel(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) {

  def blockSize(c: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Vector[_]): (Int, Int, Int) = ((c.length - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, result: Vector[_]): Unit = {
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

object VectorKernel {

  val TILE_DIM = 32

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, Unit, r)
    VectorKernel.vector_r(kernel, vector, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, Unit, r)
    VectorKernel.vector_r(kernel, matrix, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Boolean, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](if (scalar) 1 else 0)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Boolean, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Byte, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Byte, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Char, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Char](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Char, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Short, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Short, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Int, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Int, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Long, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Long, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Float, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Float, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Double, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar: Double, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, vector, scalar, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Boolean, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Boolean, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Byte, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Byte, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Char, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Char, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Short, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Short, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Int, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Int, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Long, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Long, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Float, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Float, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Double, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar: Double, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Boolean, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Byte](if (scalar) 1 else 0)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Boolean, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Byte, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Byte, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Char, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Char](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Char, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Short, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Short, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Int, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Int, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Long, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Long, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Float, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Float, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Double, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T], scalar: Double, r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, scalar, r)
    VectorKernel.vector_r(kernel, matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Boolean, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Boolean, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Byte, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Byte, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Char, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Char, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Short, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Short, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Int, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Int, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Long, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Long, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Float, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Float, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Double, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], matrix, scalar, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T], scalar: Double, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], matrix, scalar, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Boolean, scalar2: Boolean, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](if (scalar1) 1 else 0)), Pointer.to(Array[Byte](if (scalar2) 1 else 0)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Boolean, scalar2: Boolean, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Byte, scalar2: Byte, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](scalar1)), Pointer.to(Array[Byte](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Byte, scalar2: Byte, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Char, scalar2: Char, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Char](scalar1)), Pointer.to(Array[Char](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Char, scalar2: Char, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Short, scalar2: Short, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Short](scalar1)), Pointer.to(Array[Short](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Short, scalar2: Short, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Int, scalar2: Int, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Int](scalar1)), Pointer.to(Array[Int](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Int, scalar2: Int, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Long, scalar2: Long, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Long](scalar1)), Pointer.to(Array[Long](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Long, scalar2: Long, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Float, scalar2: Float, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Float](scalar1)), Pointer.to(Array[Float](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Float, scalar2: Float, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Double, scalar2: Double, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Double](scalar1)), Pointer.to(Array[Double](scalar2)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T], scalar1: Double, scalar2: Double, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (scalar1, scalar2), r)
    VectorKernel.vector_r(kernel, vector, scalar1, scalar2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Boolean, scalar2: Boolean, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Boolean, scalar2: Boolean, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Byte, scalar2: Byte, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Byte, scalar2: Byte, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Char, scalar2: Char, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Char, scalar2: Char, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Short, scalar2: Short, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Short, scalar2: Short, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Int, scalar2: Int, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Int, scalar2: Int, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Long, scalar2: Long, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Long, scalar2: Long, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Float, scalar2: Float, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Float, scalar2: Float, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Double, scalar2: Double, result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T], vector, scalar1, scalar2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T], scalar1: Double, scalar2: Double, r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T], vector, scalar1, scalar2, r)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector1: Vector[T1], vector2: Vector[T2], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector1.deviceDataPtr), Pointer.to(vector2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector1.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector1: Vector[T1], vector2: Vector[T2], r: => Vector[R]): Vector[R] = {
    val result = vector1.result(kernel, vector2, r)
    VectorKernel.vector_r(kernel, vector1, vector2, result)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector1: Vector[T1], vector2: Vector[T2], result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T1], vector1, vector2, result)
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector1: Vector[T1], vector2: Vector[T2], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T1], vector1, vector2, r)
  }

  def vector2_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector1: Vector[T1], vector2: Vector[T2], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector1.deviceDataPtr), Pointer.to(vector2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector1.length)), Pointer.to(Array[Int](vector2.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector2[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector1: Vector[T1], vector2: Vector[T2], r: => Vector[R]): Vector[R] = {
    val result = vector1.result(kernel, vector2, r)
    VectorKernel.vector2_r(kernel, vector1, vector2, result)
  }

  def vector2_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector1: Vector[T1], vector2: Vector[T2], result: Vector[R]): Vector[R] = {
    VectorKernel.vector2_r(template.kernel[T1], vector1, vector2, result)
  }

  def vector2[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector1: Vector[T1], vector2: Vector[T2], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector2(template.kernel[T1], vector1, vector2, r)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T1], matrix: Matrix[T2], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, vector: Vector[T1], matrix: Matrix[T2], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, matrix, r)
    VectorKernel.vector_r(kernel, vector, matrix, result)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T1], matrix: Matrix[T2], result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T1], vector, matrix, result)
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], vector: Vector[T1], matrix: Matrix[T2], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T1], vector, matrix, r)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T1], vector: Vector[T2], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: VectorKernel, matrix: Matrix[T1], vector: Vector[T2], r: => Vector[R]): Vector[R] = {
    val result = matrix.result(kernel, vector, r)
    VectorKernel.vector_r(kernel, matrix, vector, r)
  }

  def vector_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T1], vector: Vector[T2], result: Vector[R]): Vector[R] = {
    VectorKernel.vector_r(template.kernel[T1], matrix, vector, result)
  }

  def vector[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: VectorKernel], matrix: Matrix[T1], vector: Vector[T2], r: => Vector[R]): Vector[R] = {
    VectorKernel.vector(template.kernel[T1], matrix, vector, r)
  }

}
