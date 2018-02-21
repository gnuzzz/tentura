package ru.albemuth.tentura.tensor.kernel

import jcuda.driver.CUfunction
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class VectorMulMatrix extends VectorKernel {

  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Vector", KernelRegistry.classifier(this), "vectorMulMatrix")
//  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/VectorBase", KernelRegistry.classifier(this), "vectorMulMatrix")

}
