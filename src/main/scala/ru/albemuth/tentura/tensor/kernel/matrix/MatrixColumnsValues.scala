package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixColumnsValues(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[MatrixColumnsValues] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/MatrixColumnsValues", KernelRegistry.classifier(classOf[MatrixColumnsValues]), "matrixColumnsValues")
  }

  def materialize(functionImplName: String): MatrixColumnsValues = new MatrixColumnsValues(moduleName, classifier, functionImplName)

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (1, TILE_DIM, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = (1, (c.length - 1) / TILE_DIM + 1, 1)

}
