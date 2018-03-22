package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ScalarSubMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[ScalarSubMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[ScalarSubMatrix]), "scalarSubMatrix")
  }

  def materialize(functionImplName: String): ScalarSubMatrix = new ScalarSubMatrix(moduleName, classifier, functionImplName)

}
