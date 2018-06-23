package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class RowAddMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[RowAddMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Row", KernelRegistry.classifier(classOf[RowAddMatrix]), "rowAddMatrix")
  }

  def materialize(functionImplName: String): RowAddMatrix = new RowAddMatrix(moduleName, classifier, functionImplName)

}
