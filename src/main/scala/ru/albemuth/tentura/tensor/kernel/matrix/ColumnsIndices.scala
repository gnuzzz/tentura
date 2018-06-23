package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnsIndices(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ColumnsIndices] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Indices", KernelRegistry.classifier(classOf[ColumnsIndices]), "columnsIndices")
  }

  def materialize(functionImplName: String): ColumnsIndices = new ColumnsIndices(moduleName, classifier, functionImplName)

}
