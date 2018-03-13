package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ScalarDivMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ScalarDivMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[ScalarDivMatrix]), "scalarDivMatrix")
  }

  def materialize(functionImplName: String): ScalarDivMatrix = new ScalarDivMatrix(moduleName, classifier, functionImplName)

}
