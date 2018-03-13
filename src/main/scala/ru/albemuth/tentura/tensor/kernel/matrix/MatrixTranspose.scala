package ru.albemuth.tentura.tensor.kernel.matrix

import MatrixKernel.TILE_DIM
import MatrixTranspose.BLOCK_ROWS
import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Matrix
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixTranspose.BLOCK_ROWS

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixTranspose(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixTranspose] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[MatrixTranspose]), "matrixTranspose")
  }

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, BLOCK_ROWS, 1)

  def materialize(functionImplName: String): MatrixTranspose = new MatrixTranspose(moduleName, classifier, functionImplName)

}

object MatrixTranspose {
  val BLOCK_ROWS = 4
}