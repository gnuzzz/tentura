package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixElementWiseMulMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixElementWiseMulMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Matrix", KernelRegistry.classifier(classOf[MatrixElementWiseMulMatrix]), "matrixElementWiseMulMatrix")
  }

  def materialize(functionImplName: String): MatrixElementWiseMulMatrix = new MatrixElementWiseMulMatrix(moduleName, classifier, functionImplName)

}
