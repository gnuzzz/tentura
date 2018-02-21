package ru.albemuth.tentura.tensor.kernel

import MatrixKernel.{BLOCK_ROWS, TILE_DIM}
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.tensor.Matrix

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class MatrixKernel extends JCudaKernel {

  def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, TILE_DIM, 1)
//  def blockSize(c: Matrix[_]): (Int, Int, Int) = (TILE_DIM, BLOCK_ROWS, 1)

  def gridSize(c: Matrix[_]): (Int, Int, Int) =
    ((c.columns - 1) / TILE_DIM + 1, (c.rows - 1) / TILE_DIM + 1, 1)

  def launch(params: Pointer, result: Matrix[_]): Unit = {
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

object MatrixKernel {
  val TILE_DIM = 32
  val BLOCK_ROWS = 4
}