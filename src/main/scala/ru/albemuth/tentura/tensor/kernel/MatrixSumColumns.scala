package ru.albemuth.tentura.tensor.kernel

import jcuda.driver.CUfunction
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.VectorKernel.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixSumColumns extends VectorKernel {

  override val function: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Matrix", KernelRegistry.classifier(this), "matrixSumColumns")

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (1, TILE_DIM, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = (1, (c.length - 1) / TILE_DIM + 1, 1)

}
