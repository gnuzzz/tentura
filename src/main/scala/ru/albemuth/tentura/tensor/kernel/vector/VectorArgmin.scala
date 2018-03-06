package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorArgmin.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorArgmin(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorArgmin] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Argmin", KernelRegistry.classifier(classOf[VectorArgmin]), "argmin")
  }


  override def materialize(functionImplName: String): VectorArgmin = new VectorArgmin(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object VectorArgmin {
  val TILE_DIM = 1024
}

