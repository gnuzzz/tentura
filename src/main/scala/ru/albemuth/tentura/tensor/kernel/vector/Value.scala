package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Value(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[Value] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Values", KernelRegistry.classifier(classOf[Value]), "value")
  }

  override def materialize(functionImplName: String): Value = new Value(moduleName, classifier, functionImplName)

}
