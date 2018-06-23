package ru.albemuth.tentura.tensor.kernel.vector

import jcuda.Pointer
import ru.albemuth.tentura.kernel.JCudaKernel.pointer
import ru.albemuth.tentura.kernel.{KernelRegistry, KernelTemplate, Template}
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Operator.Operator
import ru.albemuth.tentura.tensor.Vector

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Cas(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[Cas] {

  def this(moduleName: String, function: String) {
    this("ru/albemuth/tentura/tensor/kernel/vector/" + moduleName, KernelRegistry.classifier(classOf[Cas]), function)
  }

  def materialize(functionImplName: String): Cas = new Cas(moduleName, classifier, functionImplName)

}

object Cas {

  import mutable.Map

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), pointer(Array[T](threshold)),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), pointer(Array[T](operand)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), pointer(Array[T](operand1)),
      Pointer.to(Array[Int](operator2.id)), pointer(Array[T](operand2)),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator.id)), Pointer.to(operand.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector_r[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[R]): Vector[R] = {
    val params = Pointer.to(
      Pointer.to(vector.deviceDataPtr),
      Pointer.to(Array[Int](comparator.id)), Pointer.to(threshold.deviceDataPtr),
      Pointer.to(Array[Int](operator1.id)), Pointer.to(operand1.deviceDataPtr),
      Pointer.to(Array[Int](operator2.id)), Pointer.to(operand2.deviceDataPtr),
      Pointer.to(result.deviceDataPtr),
      Pointer.to(Array[Int](vector.length))
    )

    kernel.launch(params, result)

    result
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator, operand), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](kernel: Cas, vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Vector[R]): Vector[R] = {
    val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), r)
    Cas.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator, operand, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator, operand, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator, operand, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, r: => Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator, operand, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], r: =>Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator, operand, r)
  }

  def vector_r[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[R]): Vector[R] = {
    Cas.vector_r(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
  }

  def vector[T: ClassTag, R: ClassTag](template: KernelTemplate[_ <: Cas], vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], r: =>Vector[R]): Vector[R] = {
    Cas.vector(template.kernel[T], vector, comparator, threshold, operator1, operand1, operator2, operand2, r)
  }

}
