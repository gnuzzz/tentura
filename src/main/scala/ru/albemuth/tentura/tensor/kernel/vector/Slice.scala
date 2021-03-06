package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Slice(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[Slice] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Values", KernelRegistry.classifier(classOf[Slice]), "slice")
  }

  def materialize(functionImplName: String): Slice = new Slice(moduleName, classifier, functionImplName)

}
