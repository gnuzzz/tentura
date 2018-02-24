package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.kernel.dev.{Block, Dim, Idx}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVectorKernel extends FunSuite {

  val TILE_WIDTH = 3

  test("matrixMulVector") {
    //kernel params
    val matrixRows = 8
    val matrixColumns = 7
    val matrix = Array(
      1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f,
      1.1f, 2.1f, 3.1f, 4.1f, 5.1f, 6.1f, 7.1f,
      1.2f, 2.2f, 3.2f, 4.2f, 5.2f, 6.2f, 7.2f,
      1.3f, 2.3f, 3.3f, 4.3f, 5.3f, 6.3f, 7.3f,
      1.4f, 2.4f, 3.4f, 4.4f, 5.4f, 6.4f, 7.4f,
      1.5f, 2.5f, 3.5f, 4.5f, 5.5f, 6.5f, 7.5f,
      1.6f, 2.6f, 3.6f, 4.6f, 5.6f, 6.6f, 7.6f,
      1.7f, 2.7f, 3.7f, 4.7f, 5.7f, 6.7f, 7.7f
    )

    val vectorLength = 6
    val vector = Array(1.0f, 2.0f, 3.0f, 4.0f, 5.0f, 6.0f, 7.0f)

    val resultLength = 8
    val result = NativeVector.emptyVectorData(resultLength)

    //kernel helper vars
    val matrixTile = NativeMatrix.emptyMatrixData[Float](TILE_WIDTH, TILE_WIDTH)
    val vectorTile = NativeVector.vectorData(TILE_WIDTH)

    val block = new Block(Idx(2, 0, 0), Dim(TILE_WIDTH, 1, 1))

    //main kernel code
    block.run((blockIdx, blockDim, threadIdx) => {
      val bx = blockIdx.x
      val tx = threadIdx.x

      val baseRow = bx * blockDim.x
      val index = baseRow + tx
      var resultValue = 0.0f

      for (t <- 0 until (matrixColumns - 1) / TILE_WIDTH + 1) {
        val column = t * TILE_WIDTH + tx
        if (column < matrixColumns) {
          vectorTile(tx) = vector(column)
          for (i <- 0 until TILE_WIDTH) {
            val row = baseRow + i
            if (row < matrixRows) {
              matrixTile(i)(tx) = matrix(row * matrixColumns + column)
            } else {
              matrixTile(i)(tx) = 0.0f
            }
          }
        } else {
          vectorTile(tx) = 0.0f
          for (i <- 0 until TILE_WIDTH) {
            matrixTile(i)(tx) = 0.0f
          }
        }
        block.syncThreads()

        for (i <- 0 until TILE_WIDTH) {
          resultValue += matrixTile(tx)(i) * vectorTile(i)
        }
        block.syncThreads()
      }

      if (index < resultLength) {
        result(index) = resultValue
      }

      block.syncThreads()
    })

    println(result)
  }

}
