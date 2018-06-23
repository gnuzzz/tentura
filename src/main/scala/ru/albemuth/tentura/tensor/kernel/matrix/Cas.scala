package ru.albemuth.tentura.tensor.kernel.matrix

import jcuda.Pointer
import ru.albemuth.tentura.kernel.JCudaKernel.pointer
import ru.albemuth.tentura.kernel.{KernelRegistry, KernelTemplate, Template}
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.{Matrix, Vector}
import ru.albemuth.tentura.tensor.Operator.Operator

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Cas (override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[Cas] {

  def this(module: String, function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/" + module, KernelRegistry.classifier(classOf[Cas]), function)
  }

  def materialize(functionImplName: String): Cas = new Cas(moduleName, classifier, functionImplName)

  override def gridSize(c: Matrix[_]): (Int, Int, Int) = ((c.columns - 1) / Cas.TILE_WIDTH + 1, (c.rows - 1) / Cas.TILE_HEIGHT + 1, 1)

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (Cas.TILE_WIDTH, Cas.BLOCK_ROWS, 1)

}

object Cas {

  val TILE_HEIGHT = 8
  val BLOCK_ROWS = 2
  val TILE_WIDTH = 64

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator1, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns))
    )

    kernel.launch(params, result)

    result
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator, operand, result)
  }

  def matrix[T: ClassTag, R: ClassTag](kernel: Cas, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    val result = matrix.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.matrix_r(kernel, matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], result: Matrix[R]): Matrix[R] = {
    Cas.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator, operand, r)
  }

  def matrix[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], r: => Matrix[R]): Matrix[R] = {
    Cas.matrix(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

}