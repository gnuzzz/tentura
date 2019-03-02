package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.matrix.SumColumns.TILE_DIM
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SumColumns(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SumColumns] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/SumColumns", KernelRegistry.classifier(classOf[SumColumns]), "sumColumns")
  }

  def materialize(functionImplName: String): SumColumns = new SumColumns(moduleName, classifier, functionImplName)

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = (c.length, 1, 1)

}

object SumColumns {

  val TILE_DIM = 128

}