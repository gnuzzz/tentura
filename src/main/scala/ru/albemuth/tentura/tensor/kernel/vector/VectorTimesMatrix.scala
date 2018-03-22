package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorTimesMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorTimesMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/VectorTimesMatrix", KernelRegistry.classifier(classOf[VectorTimesMatrix]), "vectorTimesMatrix")
  }

  def materialize(functionImplName: String): VectorTimesMatrix = new VectorTimesMatrix(moduleName, classifier, functionImplName)

}
