package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorColumnSubMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorColumnSubMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[VectorColumnSubMatrix]), "vectorColumnSubMatrix")
  }

  def materialize(functionImplName: String): VectorColumnSubMatrix = new VectorColumnSubMatrix(moduleName, classifier, functionImplName)

}
