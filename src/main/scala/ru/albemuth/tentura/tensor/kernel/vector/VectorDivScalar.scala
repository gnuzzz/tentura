package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorDivScalar(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorDivScalar] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(classOf[VectorDivScalar]), "vectorDivScalar")
  }

  def materialize(functionImplName: String): VectorDivScalar = new VectorDivScalar(moduleName, classifier, functionImplName)

}
