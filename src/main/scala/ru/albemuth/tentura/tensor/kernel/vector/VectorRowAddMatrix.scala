package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorRowAddMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorRowAddMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[VectorRowAddMatrix]), "vectorRowAddMatrix")
  }

  def materialize(functionImplName: String): VectorRowAddMatrix = new VectorRowAddMatrix(moduleName, classifier, functionImplName)

}
