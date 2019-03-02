package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Indices(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[Indices] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Values", KernelRegistry.classifier(classOf[Indices]), "vectorIndices")
  }

  def materialize(functionImplName: String): Indices = new Indices(moduleName, classifier, functionImplName)

}
