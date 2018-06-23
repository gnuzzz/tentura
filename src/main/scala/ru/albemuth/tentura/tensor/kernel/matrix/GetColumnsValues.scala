package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class GetColumnsValues(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[GetColumnsValues] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/ColumnsValues", KernelRegistry.classifier(classOf[GetColumnsValues]), "getColumnsValues")
  }

  def materialize(functionImplName: String): GetColumnsValues = new GetColumnsValues(moduleName, classifier, functionImplName)

}
