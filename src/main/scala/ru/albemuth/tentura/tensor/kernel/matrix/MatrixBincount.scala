package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Matrix
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixBincount.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixBincount(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixBincount] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Bincount", KernelRegistry.classifier(classOf[MatrixBincount]), "bincount")
  }

  def materialize(functionImplName: String): MatrixBincount = new MatrixBincount(moduleName, classifier, functionImplName)

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  override def gridSize(c: Matrix[_]): (Int, Int, Int) = (if (c.rows < TILE_DIM) c.rows else TILE_DIM, 1, 1)

}

object MatrixBincount {
  val TILE_DIM = 1024
}