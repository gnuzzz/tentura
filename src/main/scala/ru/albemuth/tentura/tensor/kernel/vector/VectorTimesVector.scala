package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorTimesVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorTimesVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2VectorElementwise", KernelRegistry.classifier(classOf[VectorTimesVector]), "vectorTimesVector")
  }

  def materialize(functionImplName: String): VectorTimesVector = new VectorTimesVector(moduleName, classifier, functionImplName)

}
