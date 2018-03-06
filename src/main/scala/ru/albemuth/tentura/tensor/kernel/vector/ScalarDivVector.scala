package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ScalarDivVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[ScalarDivVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[ScalarDivVector]), "scalarDivVector")
  }

  def materialize(functionImplName: String): ScalarDivVector = new ScalarDivVector(moduleName, classifier, functionImplName)

}
