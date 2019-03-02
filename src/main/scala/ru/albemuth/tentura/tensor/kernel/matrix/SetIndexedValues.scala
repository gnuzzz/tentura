package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SetIndexedValues(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[SetIndexedValues] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/IndexedValues", KernelRegistry.classifier(classOf[SetIndexedValues]), "setIndexedValues")
  }

  def materialize(functionImplName: String): SetIndexedValues = new SetIndexedValues(moduleName, classifier, functionImplName)

}
