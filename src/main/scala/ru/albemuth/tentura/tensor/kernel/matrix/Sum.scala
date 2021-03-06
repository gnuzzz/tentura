package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Scalar
import ru.albemuth.tentura.tensor.kernel.matrix.Sum.TILE_DIM
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Sum(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[Sum] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Sum", KernelRegistry.classifier(classOf[Sum]), "sum")
  }

  def materialize(functionImplName: String): Sum = new Sum(moduleName, classifier, functionImplName)

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

}

object Sum {

  val TILE_DIM = 1024

}