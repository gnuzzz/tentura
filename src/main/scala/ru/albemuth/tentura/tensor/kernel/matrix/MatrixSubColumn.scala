package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSubColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixSubColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixSubColumn]), "matrixSubColumn")
  }

  def materialize(functionImplName: String): MatrixSubColumn = new MatrixSubColumn(moduleName, classifier, functionImplName)

}
