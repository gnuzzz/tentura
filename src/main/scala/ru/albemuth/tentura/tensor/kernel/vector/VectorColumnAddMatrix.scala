package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorColumnAddMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorColumnAddMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[VectorColumnAddMatrix]), "vectorColumnAddMatrix")
  }

  def materialize(functionImplName: String): VectorColumnAddMatrix = new VectorColumnAddMatrix(moduleName, classifier, functionImplName)

}
