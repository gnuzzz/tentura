package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixRows(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixRows] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Values", KernelRegistry.classifier(classOf[MatrixRows]), "matrixRows")
  }

  def materialize(functionImplName: String): MatrixRows = new MatrixRows(moduleName, classifier, functionImplName)

}
