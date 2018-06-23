package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixColumnsIndices(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixColumnsIndices] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[MatrixAddScalar]), "matrixColumnsIndices")
  }

  def materialize(functionImplName: String): MatrixColumnsIndices = new MatrixColumnsIndices(moduleName, classifier, functionImplName)

}
