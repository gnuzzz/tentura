package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixMax.TILE_DIM
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixMaxColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[MatrixMaxColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/MaxColumn", KernelRegistry.classifier(classOf[MatrixMaxColumn]), "maxColumn")
  }

  def materialize(functionImplName: String): MatrixMaxColumn = new MatrixMaxColumn(moduleName, classifier, functionImplName)

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = (if (c.length < TILE_DIM) c.length else TILE_DIM, 1, 1)

}

object MatrixMaxColumn {
  val TILE_DIM = 1024
}
