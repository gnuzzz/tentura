package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class RowTimesMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[RowTimesMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Row", KernelRegistry.classifier(classOf[RowTimesMatrix]), "rowTimesMatrix")
  }

  def materialize(functionImplName: String): RowTimesMatrix = new RowTimesMatrix(moduleName, classifier, functionImplName)

}
