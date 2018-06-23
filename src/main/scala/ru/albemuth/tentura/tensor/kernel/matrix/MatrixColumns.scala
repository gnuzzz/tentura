package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixColumns(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixColumns] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Values", KernelRegistry.classifier(classOf[MatrixColumns]), "matrixColumns")
  }

  def materialize(functionImplName: String): MatrixColumns = new MatrixColumns(moduleName, classifier, functionImplName)

}
