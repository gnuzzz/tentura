package ru.albemuth.tentura.tensor.kernel.vector

import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Vector
import ru.albemuth.tentura.tensor.kernel.vector.Reverse.TILE_DIM

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Reverse(override val moduleName: String, override val classifier: String, override val functionName: String) extends VectorKernel(moduleName, classifier, functionName) with Template[Reverse] {

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/vector/Reverse", KernelRegistry.classifier(classOf[Reverse]), "reverse")
  }

  def materialize(functionImplName: String): Reverse = new Reverse(moduleName, classifier, functionImplName)

  private val threadsPerBlock = TILE_DIM

  override def blockSize(c: Vector[_]): (Int, Int, Int) = (threadsPerBlock, 1, 1)

  override def gridSize(c: Vector[_]): (Int, Int, Int) = ((((c.length - 1) / threadsPerBlock) / JCudaKernel.numberOfSMs + 1) * JCudaKernel.numberOfSMs, 1, 1)

//  override def launch(params: Pointer, result: Vector[_]): Unit = {
//    val block = blockSize(result)
//    val grid = gridSize(result)
//
//    JCudaDriver.cuLaunchKernel(
//      function,
//      grid._1, grid._2, grid._3,
//      block._1, block._2, block._3,
//      2 * block._1 * result.itemSizeOf(), null, // Shared memory size and stream
//      params, null // Kernel- and extra parameters
//    )
//    JCudaDriver.cuCtxSynchronize
//  }

}

object Reverse {
  val TILE_DIM = 1024
}