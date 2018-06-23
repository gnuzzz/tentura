package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.Mean.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Mean(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[Mean] {

  def this(function: String) {
    this("ru/albemuth/tentura/tensor/kernel/vector/Mean", KernelRegistry.classifier(classOf[Mean]), function)
  }

  def materialize(functionImplName: String): Mean = new Mean(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

}

object Mean {
  val TILE_DIM = 1024
}
