package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorElementWiseMulVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorElementWiseMulVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[VectorElementWiseMulVector]), "vectorElementWiseMulVector")
  }

  def materialize(functionImplName: String): VectorElementWiseMulVector = new VectorElementWiseMulVector(moduleName, classifier, functionImplName)

}
