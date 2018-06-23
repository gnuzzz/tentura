package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class RowDivMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[RowDivMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Row", KernelRegistry.classifier(classOf[RowDivMatrix]), "rowDivMatrix")
  }

  def materialize(functionImplName: String): RowDivMatrix = new RowDivMatrix(moduleName, classifier, functionImplName)

}
