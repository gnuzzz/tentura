package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorAddScalar(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorAddScalar] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(classOf[VectorAddScalar]), "vectorAddScalar")
  }

  def materialize(functionImplName: String): VectorAddScalar = new VectorAddScalar(moduleName, classifier, functionImplName)

}
