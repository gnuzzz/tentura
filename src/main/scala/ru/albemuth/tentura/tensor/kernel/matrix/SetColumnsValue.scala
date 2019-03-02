package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SetColumnsValue(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SetColumnsValue] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/ColumnsValues", KernelRegistry.classifier(classOf[SetColumnsValue]), "setColumnsValue")
  }

  def materialize(functionImplName: String): SetColumnsValue = new SetColumnsValue(moduleName, classifier, functionImplName)

}
