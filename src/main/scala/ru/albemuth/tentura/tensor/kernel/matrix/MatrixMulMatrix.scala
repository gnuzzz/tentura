package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixMulMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixMulMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixMulMatrix]), "matrixMulMatrix")
  }

  def materialize(functionImplName: String): MatrixMulMatrix = new MatrixMulMatrix(moduleName, classifier, functionImplName)

}
