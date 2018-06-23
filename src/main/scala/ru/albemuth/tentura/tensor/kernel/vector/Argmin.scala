package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.Argmin.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Argmin(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[Argmin] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Argmin", KernelRegistry.classifier(classOf[Argmin]), "argmin")
  }


  override def materialize(functionImplName: String): Argmin = new Argmin(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object Argmin {
  val TILE_DIM = 1024
}

