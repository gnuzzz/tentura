package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixMax.TILE_DIM
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixMax(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[MatrixMax] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Max", KernelRegistry.classifier(classOf[Sum]), "max")
  }

  def materialize(functionImplName: String): MatrixMax = new MatrixMax(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object MatrixMax {
  val TILE_DIM = 1024
}
