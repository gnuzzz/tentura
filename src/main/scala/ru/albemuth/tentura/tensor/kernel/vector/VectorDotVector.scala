package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorDotVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorDotVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2VectorTensor", KernelRegistry.classifier(classOf[VectorDotVector]), "vectorDotVector")
  }

  def materialize(functionImplName: String): VectorDotVector = new VectorDotVector(moduleName, classifier, functionImplName)

}
