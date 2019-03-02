package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnDotRow(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ColumnDotRow] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector2VectorTensor", KernelRegistry.classifier(classOf[ColumnDotRow]), "columnDotRow")
  }

  def materialize(functionImplName: String): ColumnDotRow = new ColumnDotRow(moduleName, classifier, functionImplName)

}
