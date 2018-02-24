package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[MatrixColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixColumn]), "matrixColumn")
  }

  def materialize(functionImplName: String): MatrixColumn = new MatrixColumn(moduleName, classifier, functionImplName)

}
