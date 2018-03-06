package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorMin.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorSum(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorSum] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Sum", KernelRegistry.classifier(classOf[VectorSum]), "sum")
  }

  def materialize(functionImplName: String): VectorSum = new VectorSum(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

}

object VectorSum {
  val TILE_DIM = 1024
}