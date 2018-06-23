package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ColumnsMean(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[ColumnsMean] {

  def this(function: String) {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Mean", KernelRegistry.classifier(classOf[ColumnsMean]), function)
  }

  def materialize(functionImplName: String): ColumnsMean = new ColumnsMean(moduleName, classifier, functionImplName)

}
