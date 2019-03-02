package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class GetColumn(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[GetColumn] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Column", KernelRegistry.classifier(classOf[GetColumn]), "getColumn")
  }

  def materialize(functionImplName: String): GetColumn = new GetColumn(moduleName, classifier, functionImplName)

}
