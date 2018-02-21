package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorSum(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorSum] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(classOf[VectorSum]), "vectorSum")
  }

  def materialize(functionImplName: String): VectorSum = new VectorSum(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (1, 1, 1)

}
