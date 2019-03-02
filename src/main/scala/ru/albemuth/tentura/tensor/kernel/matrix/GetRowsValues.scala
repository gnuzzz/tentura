package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class GetRowsValues(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[GetRowsValues] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/RowsValues", KernelRegistry.classifier(classOf[GetRowsValues]), "getRowsValues")
  }

  def materialize(functionImplName: String): GetRowsValues = new GetRowsValues(moduleName, classifier, functionImplName)

}
