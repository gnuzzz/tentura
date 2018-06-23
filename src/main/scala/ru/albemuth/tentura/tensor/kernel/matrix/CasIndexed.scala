package ru.albemuth.tentura.tensor.kernel.matrix

import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.JCudaKernel.pointer
import ru.albemuth.tentura.kernel.{GenericKernel, KernelRegistry, KernelTemplate, Template}
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.{Matrix, Vector}
import ru.albemuth.tentura.tensor.Operator.Operator
import ru.albemuth.tentura.tensor.kernel.matrix.CasIndexed.TILE_DIM

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class CasIndexed(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) with Template[CasIndexed] {

  def this(module: String, function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/" + module, KernelRegistry.classifier(classOf[CasIndexed]), function)
  }

  def materialize(functionImplName: String): CasIndexed = new CasIndexed(moduleName, classifier, functionImplName)

  def blockSize(c: Vector[Int]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Vector[Int]): (Int, Int, Int) = ((c.length - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, rows: Vector[Int], columns: Vector[Int], result: Matrix[_]): Unit = {
    val block = blockSize(rows)
    val grid = gridSize(rows)

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

object CasIndexed {

  val TILE_DIM = 32

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](operand.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](operand1.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](operand.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](operand1.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator: Operator, operand: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Matrix[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    val params = Pointer.to(
      Pointer.to(matrix.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(rows.deviceDataPtr), Pointer.to(columns.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](matrix.rows)), Pointer.to(Array[Int](matrix.columns)),
      Pointer.to(Array[Int](threshold.length))
    )

    kernel.launch(params, rows, columns, result)

    result
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator, operand, rows, columns, result)
  }

  def matrix_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], matrix: Matrix[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Matrix[T], operator2: Operator, operand2: Matrix[T], rows: Vector[Int], columns: Vector[Int], result: Matrix[R]): Matrix[R] = {
    CasIndexed.matrix_r(template.kernel[T], matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, result)
  }

}
