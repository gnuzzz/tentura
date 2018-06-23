package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Matrix
import ru.albemuth.tentura.tensor.kernel.matrix.Matrix2ScalarElementwise.{BLOCK_ROWS, TILE_HEIGHT, TILE_WIDTH}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Matrix2ScalarElementwise(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[Matrix2ScalarElementwise] {

  def this(function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix2Scalar", KernelRegistry.classifier(classOf[Matrix2ScalarElementwise]), function)
  }

  def materialize(functionImplName: String): Matrix2ScalarElementwise = new Matrix2ScalarElementwise(moduleName, classifier, functionImplName)

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_WIDTH, BLOCK_ROWS, 1)

  override def gridSize(c: Matrix[_]): (Int, Int, Int) = ((c.columns - 1) / TILE_WIDTH + 1, (c.rows - 1) / TILE_HEIGHT + 1, 1)

}

object Matrix2ScalarElementwise {

  val TILE_HEIGHT = 8
  val BLOCK_ROWS = 2
  val TILE_WIDTH = 64

}