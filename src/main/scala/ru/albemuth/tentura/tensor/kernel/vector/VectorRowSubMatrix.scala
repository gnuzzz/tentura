package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorRowSubMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorRowSubMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[VectorRowSubMatrix]), "vectorRowSubMatrix")
  }

  def materialize(functionImplName: String): VectorRowSubMatrix = new VectorRowSubMatrix(moduleName, classifier, functionImplName)

}
