package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorMatrixMulVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorMatrixMulVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[VectorMatrixMulVector]), "vectorMatrixMulVector")
  }

  def materialize(functionImplName: String): VectorMatrixMulVector = new VectorMatrixMulVector(moduleName, classifier, functionImplName)

}
