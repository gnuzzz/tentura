package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnDivMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ColumnDivMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[ColumnDivMatrix]), "columnDivMatrix")
  }

  def materialize(functionImplName: String): ColumnDivMatrix = new ColumnDivMatrix(moduleName, classifier, functionImplName)

}
