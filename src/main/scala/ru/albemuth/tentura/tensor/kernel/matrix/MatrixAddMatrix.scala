package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixAddMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixAddMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[MatrixAddMatrix]), "matrixAddMatrix")
  }

  def materialize(functionImplName: String): MatrixAddMatrix = new MatrixAddMatrix(moduleName, classifier, functionImplName)

}