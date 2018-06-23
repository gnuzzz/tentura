package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class RowsMean(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[RowsMean] {

  def this(function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Mean", KernelRegistry.classifier(classOf[RowsMean]), function)
  }

  def materialize(functionImplName: String): RowsMean = new RowsMean(moduleName, classifier, functionImplName)

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (1, TILE_DIM, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = (1, (c.length - 1) / TILE_DIM + 1, 1)

}
