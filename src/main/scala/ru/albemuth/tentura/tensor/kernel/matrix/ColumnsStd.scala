package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnsStd(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[ColumnsStd] {

  def this(function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Std", KernelRegistry.classifier(classOf[ColumnsStd]), function)
  }

  def materialize(functionImplName: String): ColumnsStd = new ColumnsStd(moduleName, classifier, functionImplName)

}
