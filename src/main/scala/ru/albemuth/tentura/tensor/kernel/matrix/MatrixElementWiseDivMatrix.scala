package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixElementWiseDivMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixElementWiseDivMatrix] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(classOf[MatrixElementWiseDivMatrix]), "matrixElementWiseDivMatrix")
  }

  def materialize(functionImplName: String): MatrixElementWiseDivMatrix = new MatrixElementWiseDivMatrix(moduleName, classifier, functionImplName)

}
