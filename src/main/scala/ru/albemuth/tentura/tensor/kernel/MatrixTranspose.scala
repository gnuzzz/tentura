package ru.albemuth.tentura.tensor.kernel

import MatrixKernel.TILE_DIM
import MatrixTranspose.BLOCK_ROWS
import jcuda.driver.CUfunction
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry}
import ru.albemuth.tentura.tensor.Matrix

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixTranspose extends MatrixKernel {

  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(this), "matrixTranspose")
//  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/MatrixBase", KernelRegistry.classifier(this), "matrixTranspose")

  override def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, BLOCK_ROWS, 1)

}

object MatrixTranspose {
  val BLOCK_ROWS = 4
}