package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorTimesVector(override val moduleName: String, override val classifier: String, override val functionName: String) extends ScalarKernel(moduleName, classifier, functionName) with Template[VectorTimesVector] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Vector", KernelRegistry.classifier(classOf[VectorTimesVector]), "vectorTimesVector")
//    this("ru/albemuth/tentura/tensor/kernel/vector/VectorBase", KernelRegistry.classifier(classOf[VectorMulVector]), "vectorMulVector")
  }

  def materialize(functionImplName: String): VectorTimesVector = new VectorTimesVector(moduleName, classifier, functionImplName)

}
