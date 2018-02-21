package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorElementWiseDivVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorElementWiseDivVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[VectorElementWiseDivVector]), "vectorElementWiseDivVector")
  }

  def materialize(functionImplName: String): VectorElementWiseDivVector = new VectorElementWiseDivVector(moduleName, classifier, functionImplName)

}
