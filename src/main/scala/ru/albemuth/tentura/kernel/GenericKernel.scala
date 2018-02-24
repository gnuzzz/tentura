package ru.albemuth.tentura.kernel
import jcuda.driver.CUfunction

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class GenericKernel(val moduleName: String, val classifier: String, val functionName: String) extends JCudaKernel {

  override lazy val function: CUfunction = JCudaKernel.loadKernel(moduleName, classifier, functionName)

}
