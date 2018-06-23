package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.Max.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Max(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[Max] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Max", KernelRegistry.classifier(classOf[Max]), "max")
  }

  override def materialize(functionImplName: String): Max = new Max(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object Max {
  val TILE_DIM = 1024
}