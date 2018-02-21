package ru.albemuth.tentura.tensor.kernel

import jcuda.driver.CUfunction
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry}
import ru.albemuth.tentura.tensor.kernel.VectorKernel.TILE_DIM
import ru.albemuth.tentura.tensor.Scalar

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSum extends ScalarKernel {

  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(this), "matrixSum")

  override def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, TILE_DIM, 1)

}
