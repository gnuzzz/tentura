package ru.albemuth.tentura.tensor.kernel.matrix

import jcuda.Pointer
import jcuda.driver.{CUfunction, JCudaDriver}
import ru.albemuth.tentura.kernel.{JCudaKernel, KernelRegistry, Template}
import ru.albemuth.tentura.tensor.Matrix
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixDotMatrix.TILE_DIM_32

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixDotMatrix(override val moduleName: String, override val classifier: String, override val functionName: String) extends MatrixKernel(moduleName, classifier, functionName) with Template[MatrixDotMatrix] {

  lazy val function8: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixDotMatrix8", classifier, functionName)
  lazy val function16: CUfunction = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixDotMatrix16", classifier, functionName)

  def this() {
    this("ru/albemuth/tentura/tensor/kernel/matrix/MatrixDotMatrix32", KernelRegistry.classifier(classOf[MatrixDotMatrix]), "matrixDotMatrix")
  }

  def materialize(functionImplName: String): MatrixDotMatrix = new MatrixDotMatrix(moduleName, classifier, functionImplName)

  override def launch(params: Pointer, result: Matrix[_]): Unit = {
    val c = result.columns - 1
    val r = result.rows - 1
    var f = function
    var tileDim = TILE_DIM_32
    var gridDimX = c / tileDim + 1
    var gridDimY = r / tileDim + 1

    JCudaDriver.cuLaunchKernel(
      f,
      gridDimX, gridDimY, 1,
      tileDim, tileDim, 1,
//      TILE_DIM * TILE_DIM * sizeOf(), null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

}

object MatrixDotMatrix {

  val TILE_DIM_8 = 8
  val TILE_DIM_16 = 16
  val TILE_DIM_32 = 32

}
