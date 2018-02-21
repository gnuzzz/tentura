package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSubScalar(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixSubScalar] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixSubScalar]), "matrixSubScalar")
  }

  def materialize(functionImplName: String): MatrixSubScalar = new MatrixSubScalar(moduleName, classifier, functionImplName)

}
