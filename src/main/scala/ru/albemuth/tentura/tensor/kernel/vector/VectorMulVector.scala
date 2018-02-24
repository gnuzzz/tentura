package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorMulVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorMulVector] {

  def this() {
//    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(this), "vectorMulVector")
    this("ru/albemuth/tentura/tensor/kernel/VectorBase", KernelRegistry.classifier(classOf[VectorMulVector]), "vectorMulVector")
  }

  def materialize(functionImplName: String): VectorMulVector = new VectorMulVector(moduleName, classifier, functionImplName)

}
