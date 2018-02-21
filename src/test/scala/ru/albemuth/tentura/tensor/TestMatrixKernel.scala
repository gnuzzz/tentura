package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.kernel.dev.{Block, Dim, Idx}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMatrixKernel extends FunSuite {

  val TILE_WIDTH = 2

  test("matrixTranspose") {
    //kernel params
    val numRows = 2
    val numColumns = 3
    val src = Array(
      1.0f, 2.0f, 3.0f,
      1.1f, 2.1f, 3.1f
    )
    val dst = new Array[Float](src.length)

    //kernel helper vars
    val srcTile = NativeMatrix.emptyMatrixData[Float](TILE_WIDTH, TILE_WIDTH)

    val block = new Block(Idx(0, 1, 0), Dim(TILE_WIDTH, TILE_WIDTH, 1))

    //main kernel code
    block.run((blockIdx, blockDim, threadIdx) => {
      val bx = blockIdx.x
      val by = blockIdx.y
      val tx = threadIdx.x
      val ty = threadIdx.y

      val baseRow = by * blockDim.y
      val baseCol = bx * blockDim.x
      val srcRow = baseCol + ty
      val srcCol = baseRow + tx
      val dstRow = baseRow + ty
      val dstCol = baseCol + tx

      if (srcRow < numRows && srcCol < numColumns) {
        srcTile(ty)(tx) = src(srcRow * numColumns + srcCol)
      } else {
        srcTile(ty)(tx) = 0.0f
      }
      block.syncThreads()

      if (dstRow < numRows && dstCol < numColumns) {
        dst(dstRow * numColumns + dstCol) = srcTile(tx)(ty)
      }
      block.syncThreads()
    })
  }


}
