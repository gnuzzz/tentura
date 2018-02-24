package ru.albemuth.tentura.tensor.kernel.math

import MathKernel.TILE_DIM
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.{GenericKernel, KernelRegistry, KernelTemplate, Template}
import ru.albemuth.tentura.tensor.{Matrix, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MathKernel(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel("", classifier, functionName) with Template[MathKernel] {

  def this(functionName: String) {
    this("ru/albemuth/tentura/tensor/kernel/Math", KernelRegistry.classifier(classOf[MathKernel]), functionName)
  }

  override def materialize(functionImplName: String): MathKernel = new MathKernel(functionImplName)

  def blockSize(v: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def blockSize(m: Matrix[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(v: Vector[_]): (Int, Int, Int) = ((v.length - 1) / TILE_DIM + 1, 1, 1)

  def gridSize(v: Matrix[_]): (Int, Int, Int) = (((v.rows * v.columns) - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, result: Vector[_]): Unit = {
    val block = blockSize(result)
    val grid = gridSize(result)

    JCudaDriver.cuLaunchKernel(
      function,
      grid._1, grid._2, grid._3,
      block._1, block._2, block._3,
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  def launch(params: Pointer, result: Matrix[_]): Unit = {
    val block = blockSize(result)
    val grid = gridSize(result)

    JCudaDriver.cuLaunchKernel(
      function,
      grid._1, grid._2, grid._3,
      block._1, block._2, block._3,
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

}

object MathKernel {

  val TILE_DIM = 32

  def vectort[T: ClassTag, R: ClassTag](kernel: MathKernel, vector: Vector[T]): Vector[R] = {
    val result = vector.result(kernel, vector, new Vector[R](vector.length))
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def vectort[T: ClassTag, R: ClassTag](kernel: MathKernel, vector: Vector[T], param: Float): Vector[R] = {
    val result = vector.result(kernel, param, new Vector[R](vector.length))
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array(param)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def vectort[T: ClassTag, R: ClassTag](kernel: MathKernel, vector: Vector[T], param: Double): Vector[R] = {
    val result = vector.result(kernel, param, new Vector[R](vector.length))
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr), Pointer.to(Array(param)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def vectort[T: ClassTag, R: ClassTag](kernel: MathKernel, vector1: Vector[T], vector2: Vector[T]): Vector[R] = {
    val result = vector1.result(kernel, vector2, new Vector[R](vector1.length))
    val params = Pointer.to(
      Pointer.to(vector1.deviceDataPtr), Pointer.to(vector2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.length))
    )

    kernel.launch(params, result)

    result
  }

  def vectort[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], vector: Vector[T]): Vector[R] = {
    vectort[T, R](template.kernel[T], vector)
  }

  def vectort[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], vector: Vector[T], param: Float): Vector[R] = {
    vectort[T, R](template.kernel[T], vector, param)
  }

  def vectort[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], vector: Vector[T], param: Double): Vector[R] = {
    vectort[T, R](template.kernel[T], vector, param)
  }

  def vectort[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], vector1: Vector[T], vector2: Vector[T]): Vector[R] = {
    vectort[T, R](template.kernel[T], vector1, vector2)
  }

  def matrixt[T: ClassTag, R: ClassTag](kernel: MathKernel, matrix: Matrix[T]): Matrix[R] = {
    val result = matrix.result(kernel, matrix, new Matrix[R](matrix.rows, matrix.columns))
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.rows * result.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrixt[T: ClassTag, R: ClassTag](kernel: MathKernel, matrix: Matrix[T], param: Float): Matrix[R] = {
    val result = matrix.result(kernel, param, new Matrix[R](matrix.rows, matrix.columns))
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array(param)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.rows * result.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrixt[T: ClassTag, R: ClassTag](kernel: MathKernel, matrix: Matrix[T], param: Double): Matrix[R] = {
    val result = matrix.result(kernel, param, new Matrix[R](matrix.rows, matrix.columns))
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr), Pointer.to(Array(param)), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.rows * result.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrixt[T: ClassTag, R: ClassTag](kernel: MathKernel, matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[R] = {
    val result = matrix1.result(kernel, matrix2, new Matrix[R](matrix1.rows, matrix1.columns))
    val params = Pointer.to(
      Pointer.to(matrix1.deviceDataPtr), Pointer.to(matrix2.deviceDataPtr), Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](result.rows * result.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrixt[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], matrix: Matrix[T]): Matrix[R] = {
    matrixt[T, R](template.kernel[T], matrix)
  }

  def matrixt[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], matrix: Matrix[T], param: Float): Matrix[R] = {
    matrixt[T, R](template.kernel[T], matrix, param)
  }

  def matrixt[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], matrix: Matrix[T], param: Double): Matrix[R] = {
    matrixt[T, R](template.kernel[T], matrix, param)
  }

  def matrixt[T: ClassTag, R: ClassTag](template: KernelTemplate[MathKernel], matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[R] = {
    matrixt[T, R](template.kernel[T], matrix1, matrix2)
  }

}
