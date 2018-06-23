package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorIndices(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorIndices] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[VectorIndices]), "vectorIndices")
  }

  def materialize(functionImplName: String): VectorIndices = new VectorIndices(moduleName, classifier, functionImplName)

}
