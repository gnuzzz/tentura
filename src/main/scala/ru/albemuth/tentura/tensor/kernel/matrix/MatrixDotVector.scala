package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixDotVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[MatrixDotVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix2VectorTensor", KernelRegistry.classifier(classOf[MatrixDotVector]), "matrixDotVector")
  }

  def materialize(functionImplName: String): MatrixDotVector = new MatrixDotVector(moduleName, classifier, functionImplName)

}
