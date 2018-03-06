package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorBincount.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorBincount(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[VectorBincount] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Bincount", KernelRegistry.classifier(classOf[VectorBincount]), "bincount")
  }

  override def materialize(functionImplName: String): VectorBincount = new VectorBincount(moduleName, classifier, functionImplName)

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = ((c.length - 1) / TILE_DIM + 1, 1, 1)

}

object VectorBincount {
  val TILE_DIM = 1024
}