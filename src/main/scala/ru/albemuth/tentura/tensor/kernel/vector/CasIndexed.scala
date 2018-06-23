package ru.albemuth.tentura.tensor.kernel.vector

import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.JCudaKernel.pointer
import ru.albemuth.tentura.kernel.{GenericKernel, KernelRegistry, KernelTemplate, Template}
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Operator.Operator
import ru.albemuth.tentura.tensor.kernel.vector.CasIndexed.TILE_DIM
import ru.albemuth.tentura.tensor.{Matrix, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class CasIndexed(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) with Template[CasIndexed] {

  def this(moduleName: String, function: String) {
    this("ru/albemuth/tentura/tensor/kernel/vector/" + moduleName, KernelRegistry.classifier(classOf[CasIndexed]), function)
  }

  def materialize(functionImplName: String): CasIndexed = new CasIndexed(moduleName, classifier, functionImplName)

  def blockSize(c: Vector[Int]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Vector[Int]): (Int, Int, Int) = ((c.length - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, indices: Vector[Int], result: Vector[_]): Unit = {
    val block = blockSize(indices)
    val grid = gridSize(indices)

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

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, indices, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, indices, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: CasIndexed, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(indices.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length)),
      Pointer.to(Array[Int](indices.length))
    )

    kernel.launch(params, indices, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, indices, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: CasIndexed], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[R]): Vector[R] = {
    CasIndexed.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
  }

}
