package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorArgmax.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorArgmax(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorArgmax] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Argmax", KernelRegistry.classifier(classOf[VectorArgmax]), "argmax")
  }

  override def materialize(functionImplName: String): VectorArgmax = new VectorArgmax(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object VectorArgmax {
  val TILE_DIM = 1024
}
