package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSubRow(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixSubRow] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[MatrixSubRow]), "matrixSubRow")
  }

  def materialize(functionImplName: String): MatrixSubRow = new MatrixSubRow(moduleName, classifier, functionImplName)

}
