package ru.albemuth.tentura.kernel.dev

import org.scalatest.FunSuite

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object SampleBlock extends App {

  val TILE_WIDTH = 16

  val matrixTile = new Array[Array[Float]](TILE_WIDTH)
  for (i <- matrixTile.indices) matrixTile(i) = new Array[Float](TILE_WIDTH)
  val vectorTile = new Array[Float](TILE_WIDTH)

  val block = new Block(Idx(1, 0, 0), Dim(2, 1, 1))

  block.run((blockIdx, blockDim, threadIdx) => {
    if (threadIdx.x % 2 == 0) Thread.sleep(1000) else Thread.sleep(3000)
    println(s"${threadIdx.x}: ${matrixTile.length}")
    block.syncThreads()
    if (threadIdx.x % 2 == 0) Thread.sleep(1000) else Thread.sleep(3000)
    println(s"${threadIdx.x}: ${matrixTile.length}")
  })

}
