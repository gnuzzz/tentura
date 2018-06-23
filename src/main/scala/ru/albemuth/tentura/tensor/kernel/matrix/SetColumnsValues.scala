package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SetColumnsValues(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SetColumnsValues] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/ColumnsValues", KernelRegistry.classifier(classOf[SetColumnsValues]), "setColumnsValues")
  }

  def materialize(functionImplName: String): SetColumnsValues = new SetColumnsValues(moduleName, classifier, functionImplName)

}
