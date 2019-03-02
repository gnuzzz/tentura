package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SumRows(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SumRows] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/SumRows", KernelRegistry.classifier(classOf[SumRows]), "sumRows")
  }

  def materialize(functionImplName: String): SumRows = new SumRows(moduleName, classifier, functionImplName)

}
