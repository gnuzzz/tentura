package ru.albemuth.tentura.tensor.kernel

import jcuda.driver.CUfunction
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry}
import ru.albemuth.tentura.tensor.Scalar

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorSum extends ScalarKernel {

  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(this), "vectorSum")

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (1, 1, 1)

}
