package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorDotMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorDotMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2MatrixTensor", KernelRegistry.classifier(classOf[VectorDotMatrix]), "vectorDotMatrix")
  }

  def materialize(functionImplName: String): VectorDotMatrix = new VectorDotMatrix(moduleName, classifier, functionImplName)

}
