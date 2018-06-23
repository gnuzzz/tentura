package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorTimesScalar(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorTimesScalar] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2Scalar", KernelRegistry.classifier(classOf[VectorTimesScalar]), "vectorTimesScalar")
  }

  def materialize(functionImplName: String): VectorTimesScalar = new VectorTimesScalar(moduleName, classifier, functionImplName)

}
