package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixTimesColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixTimesColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[MatrixTimesColumn]), "matrixTimesColumn")
  }

  def materialize(functionImplName: String): MatrixTimesColumn = new MatrixTimesColumn(moduleName, classifier, functionImplName)

}
