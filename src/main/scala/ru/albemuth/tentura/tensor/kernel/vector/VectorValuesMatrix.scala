package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorValuesMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[VectorValuesMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Values", KernelRegistry.classifier(classOf[VectorValuesMatrix]), "vectorValuesMatrix")
  }

  def materialize(functionImplName: String): VectorValuesMatrix = new VectorValuesMatrix(moduleName, classifier, functionImplName)

}
