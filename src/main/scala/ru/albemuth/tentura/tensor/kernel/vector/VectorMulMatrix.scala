package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorMulMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorMulMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(classOf[VectorMulMatrix]), "vectorMulMatrix")
  }

  def materialize(functionImplName: String): VectorMulMatrix = new VectorMulMatrix(moduleName, classifier, functionImplName)

}
