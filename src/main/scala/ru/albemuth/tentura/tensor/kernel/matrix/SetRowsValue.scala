package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SetRowsValue(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SetRowsValue] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/RowsValue", KernelRegistry.classifier(classOf[SetRowsValue]), "setRowsValue")
  }

  def materialize(functionImplName: String): SetRowsValue = new SetRowsValue(moduleName, classifier, functionImplName)

}
