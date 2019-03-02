package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SetIndexedValue(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SetIndexedValue] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/IndexedValues", KernelRegistry.classifier(classOf[SetIndexedValue]), "setIndexedValue")
  }

  def materialize(functionImplName: String): SetIndexedValue = new SetIndexedValue(moduleName, classifier, functionImplName)

}
