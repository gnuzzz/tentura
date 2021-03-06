package ru.albemuth.tentura.tensor.kernel.matrix

import ru.albemuth.tentura.kernel.{KernelRegistry, Template}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class SliceColumns(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[SliceColumns] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/Slice", KernelRegistry.classifier(classOf[SliceColumns]), "sliceColumns")
  }

  def materialize(functionImplName: String): SliceColumns = new SliceColumns(moduleName, classifier, functionImplName)

}
