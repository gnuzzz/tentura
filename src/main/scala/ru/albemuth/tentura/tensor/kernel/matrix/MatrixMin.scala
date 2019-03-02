package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixMin.TILE_DIM
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixMin(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[MatrixMin] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Min", KernelRegistry.classifier(classOf[Sum]), "min")
  }

  def materialize(functionImplName: String): MatrixMin = new MatrixMin(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object MatrixMin {
  val TILE_DIM = 1024
}
