package ru.albemuth.tentura.tensor.kernel

import VectorKernel.TILE_DIM
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.tensor.Vector

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class VectorKernel extends JCudaKernel {

  def blockSize(c: Vector[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Vector[_]): (Int, Int, Int) = ((c.length - 1) / TILE_DIM + 1, 1, 1)

  def launch(params: Pointer, result: Vector[_]): Unit = {
    val block = blockSize(result)
    val grid = gridSize(result)

    JCudaDriver.cuLaunchKernel(
      function,
      grid._1, grid._2, grid._3,
      block._1, block._2, block._3,
      //      TiledMatrixMultiplicationKernel.TILE_WIDTH * TiledMatrixMultiplicationKernel.TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

}

object VectorKernel {
  val TILE_DIM = 32
}
