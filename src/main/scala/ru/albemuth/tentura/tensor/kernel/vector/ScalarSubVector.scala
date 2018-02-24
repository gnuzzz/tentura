package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ScalarSubVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[ScalarSubVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(classOf[ScalarSubVector]), "scalarSubVector")
  }

  def materialize(functionImplName: String): ScalarSubVector = new ScalarSubVector(moduleName, classifier, functionImplName)

}
