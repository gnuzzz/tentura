package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnTimesMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ColumnTimesMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[ColumnTimesMatrix]), "columnTimesMatrix")
  }

  def materialize(functionImplName: String): ColumnTimesMatrix = new ColumnTimesMatrix(moduleName, classifier, functionImplName)

}
