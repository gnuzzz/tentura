package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Values(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[Values] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Values", KernelRegistry.classifier(classOf[Values]), "values")
  }

  def materialize(functionImplName: String): Values = new Values(moduleName, classifier, functionImplName)

}
