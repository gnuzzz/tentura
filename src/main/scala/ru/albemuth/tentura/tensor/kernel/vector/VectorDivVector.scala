package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorDivVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorDivVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2VectorElementwise", KernelRegistry.classifier(classOf[VectorDivVector]), "vectorDivVector")
  }

  def materialize(functionImplName: String): VectorDivVector = new VectorDivVector(moduleName, classifier, functionImplName)

}
