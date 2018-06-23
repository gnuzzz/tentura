package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixDivRow(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixDivRow] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Row", KernelRegistry.classifier(classOf[MatrixDivRow]), "matrixDivRow")
  }

  def materialize(functionImplName: String): MatrixDivRow = new MatrixDivRow(moduleName, classifier, functionImplName)

}
