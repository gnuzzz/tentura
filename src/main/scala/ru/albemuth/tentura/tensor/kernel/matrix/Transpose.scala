package ru.albemuth.tentura.tensor.kernel.matrix

import MatrixKernel.TILE_DIM
import Transpose.BLOCK_ROWS
import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Matrix
import ru.albemuth.tentura.tensor.kernel.matrix.Transpose.BLOCK_ROWS

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Transpose(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[Transpose] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Transpose", KernelRegistry.classifier(classOf[Transpose]), "transpose")
  }

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, BLOCK_ROWS, 1)

  def materialize(functionImplName: String): Transpose = new Transpose(moduleName, classifier, functionImplName)

}

object Transpose {
  val BLOCK_ROWS = 4
}