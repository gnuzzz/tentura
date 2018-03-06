package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorMax.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorMax(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorMax] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Max", KernelRegistry.classifier(classOf[VectorMax]), "max")
  }

  override def materialize(functionImplName: String): VectorMax = new VectorMax(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)
}

object VectorMax {
  val TILE_DIM = 1024
}