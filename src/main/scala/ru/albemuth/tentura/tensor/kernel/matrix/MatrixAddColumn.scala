package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixAddColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixAddColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[MatrixAddColumn]), "matrixAddColumn")
  }

  def materialize(functionImplName: String): MatrixAddColumn = new MatrixAddColumn(moduleName, classifier, functionImplName)

}
