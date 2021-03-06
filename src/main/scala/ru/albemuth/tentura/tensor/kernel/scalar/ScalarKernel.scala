package ru.albemuth.tentura.tensor.kernel.scalar

import ScalarKernel.TILE_DIM
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.{GenericKernel, KernelTemplate}
import ru.albemuth.tentura.tensor.{Matrix, Scalar, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class ScalarKernel(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) {

  def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def blockSize(v: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Scalar[_]): (Int, Int, Int) = (1, 1, 1)

  def gridSize(v: Vector[_]): (Int, Int, Int) = ((v.length - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, result: Scalar[_]): Unit = {
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

object ScalarKernel {

  val TILE_DIM = 32

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, Unit, r)
    ScalarKernel.scalar_r(kernel, vector, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T]): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector)
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Boolean, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](if (scalar) 1 else 0)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Boolean, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Boolean): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Byte, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Byte](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Byte, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Byte): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Char, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Char](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Char, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Char): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Short, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Short](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Short, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Short): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Int, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Int](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Int, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Int): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Long, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Long](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Long, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Long): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Float, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Float](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Float, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Float): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Double, result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array[Double](scalar)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Double, r: => Scalar[R]): Scalar[R] = {
    val result = vector.result(kernel, scalar, r)
    ScalarKernel.scalar_r(kernel, vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, vector: Vector[T], scalar: Double): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector, scalar, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Boolean, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Boolean, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Boolean): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Byte, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Byte, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Byte): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Char, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Char, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Char): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Short, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Short, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Short): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Int, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Int, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Int): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Long, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Long, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Long): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Float, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Float, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Float): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Double, result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], vector, scalar, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Double, r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector: Vector[T], scalar: Double): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], vector, scalar)
  }

  def scalar_r[T: ClassTag, R: ClassTag](kernel: ScalarKernel, matrix: Matrix[T], result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, matrix: Matrix[T], r: => Scalar[R]): Scalar[R] = {
    val result = matrix.result(kernel, Unit, r)
    ScalarKernel.scalar_r(kernel, matrix, result)
  }

  def scalar[T: ClassTag, R: ClassTag](kernel: ScalarKernel, matrix: Matrix[T]): Scalar[R] = {
    ScalarKernel.scalar(kernel, matrix, new Scalar[R]())
  }

  def scalar_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], matrix: Matrix[T], result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T], matrix, result)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], matrix: Matrix[T], r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], matrix, r)
  }

  def scalar[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], matrix: Matrix[T]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T], matrix)
  }

  def scalar_r[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: ScalarKernel, vector1: Vector[T1], vector2: Vector[T2], result: Scalar[R]): Scalar[R] = {
    val params = Pointer.to(
      Pointer.to(vector1.deviceDataPtr), Pointer.to(vector2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector1.length))
    )

    kernel.launch(params, result)

    result
  }

  def scalar[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: ScalarKernel, vector1: Vector[T1], vector2: Vector[T2], r: => Scalar[R]): Scalar[R] = {
    val result = vector1.result(kernel, vector2, r)
    ScalarKernel.scalar_r(kernel, vector1, vector2, result)
  }

  def scalar[T1: ClassTag, T2: ClassTag, R: ClassTag](kernel: ScalarKernel, vector1: Vector[T1], vector2: Vector[T2]): Scalar[R] = {
    ScalarKernel.scalar(kernel, vector1, vector2, new Scalar[R]())
  }

  def scalar_r[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector1: Vector[T1], vector2: Vector[T2], result: Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar_r(template.kernel[T1], vector1, vector2, result)
  }

  def scalar[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector1: Vector[T1], vector2: Vector[T2], r: => Scalar[R]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T1], vector1, vector2, r)
  }

  def scalar[T1: ClassTag, T2: ClassTag, R: ClassTag](template: KernelTemplate[_ <: ScalarKernel], vector1: Vector[T1], vector2: Vector[T2]): Scalar[R] = {
    ScalarKernel.scalar(template.kernel[T1], vector1, vector2)
  }

}