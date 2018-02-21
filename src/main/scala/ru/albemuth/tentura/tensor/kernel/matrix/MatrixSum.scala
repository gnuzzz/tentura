package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSum(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[MatrixSum] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixSum]), "matrixSum")
  }

  def materialize(functionImplName: String): MatrixSum = new MatrixSum(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, TILE_DIM, 1)

}
