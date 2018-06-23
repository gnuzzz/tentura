package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class RowsIndices(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[RowsIndices] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Indices", KernelRegistry.classifier(classOf[RowsIndices]), "rowsIndices")
  }

  def materialize(functionImplName: String): RowsIndices = new RowsIndices(moduleName, classifier, functionImplName)

}
