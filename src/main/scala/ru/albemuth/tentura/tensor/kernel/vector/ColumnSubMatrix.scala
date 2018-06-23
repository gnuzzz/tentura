package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnSubMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ColumnSubMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[ColumnSubMatrix]), "columnSubMatrix")
  }

  def materialize(functionImplName: String): ColumnSubMatrix = new ColumnSubMatrix(moduleName, classifier, functionImplName)

}
