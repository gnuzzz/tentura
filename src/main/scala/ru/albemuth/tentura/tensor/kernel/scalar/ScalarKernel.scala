package ru.albemuth.tentura.tensor.kernel.scalar

import ScalarKernel.TILE_DIM
import jcuda.Pointer
import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.GenericKernel
import ru.albemuth.tentura.tensor.Scalar

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class ScalarKernel(override val moduleName: String, override val classifier: String, override val functionName: String) extends GenericKernel(moduleName, classifier, functionName) {

  def blockSize(c: Scalar[_]): (Int, Int, Int) = (TILE_DIM, 1, 1)

  def gridSize(c: Scalar[_]): (Int, Int, Int) = (1, 1, 1)

  def launch(params: Pointer, result: Scalar[_]): Unit = {
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

object ScalarKernel {
  val TILE_DIM = 32
}