package ru.albemuth.tentura.performance

import jcuda.{Pointer, Sizeof}
import jcuda.driver.{CUdeviceptr, JCudaDriver}
import org.scalameter.{Key, MeasureBuilder, Warmer, config}
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel
import ru.albemuth.tentura.tensor.{Matrix, NativeMatrix, NativeVector}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait MatrixPerformance {

  val ROWS = 5120
  val COLUMNS = 1280

  val standardConfig: MeasureBuilder[Unit, Double] = config(
    Key.exec.minWarmupRuns -> 20,
    Key.exec.maxWarmupRuns -> 40,
    Key.exec.benchRuns -> 25,
    Key.verbose -> true
  ) withWarmer new Warmer.Default

}

//-Djava.library.path=D:/Vovan/lang/albemuth/tentura/lib -Xms1024m -Xmx1024m
object MatrixMulMatrix extends App with MatrixPerformance {
  //val ROWS = 512
  //val COLUMNS = 128
  //base: matrix * matrix time: 4.995856679999999 ms
  //tiled: matrix * matrix time: 2.1242496 ms
  //val ROWS = 5120
  //val COLUMNS = 1280
  //base: N/A
  //tiled: matrix * matrix time: 1924.1014661600004 ms
  val a = Matrix.of(NativeMatrix.matrixData[Float](ROWS, COLUMNS))
  val b = Matrix.of(NativeMatrix.matrixData[Float](COLUMNS, ROWS))
//  val result = a * b
//  println(s"${Runtime.getRuntime.totalMemory()}/${Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()}")

  val multiplyTime = standardConfig measure {
    val result = a * b
//    println(s"${Runtime.getRuntime.totalMemory()}/${Runtime.getRuntime.totalMemory() - Runtime.getRuntime.freeMemory()}")
  }

  println(s"native matrix * native matrix time: $multiplyTime")
}

object NativeMatrixMulNativeMatrix extends App with MatrixPerformance {
  //val ROWS = 512
  //val COLUMNS = 128
  //native matrix * matrix time: 366.2258579199999 ms
  val a = NativeMatrix.matrix(ROWS, COLUMNS)
  val b = NativeMatrix.matrix(COLUMNS, ROWS)

  val result = NativeMatrix.emptyMatrix(ROWS, ROWS)

  val multiplyTime = standardConfig measure {
    for (i <- result.data.indices) {
      val resultRow = result.data(i)
      for (j <- resultRow.indices) {
        var cij = 0.0f
        for (k <- a.data(i).indices) {
          cij += a.data(i)(k) * b.data(k)(j)
        }
        resultRow(j) = cij
      }
    }
  }

  println(s"native matrix * matrix time: $multiplyTime")
}

object RawMatrixMulMatrix extends App with MatrixPerformance {
  //val ROWS = 5120
  //val COLUMNS = 1280
  //raw matrix * matrix time: 1923.3336968400001 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/Matrix", "", "matrixMulMatrix")
  val aData = NativeVector.vectorData(ROWS * COLUMNS)
  val bData = NativeVector.vectorData(COLUMNS * ROWS)
  val resultData = NativeVector.vectorData(ROWS * ROWS)

  val blockSize = (MatrixKernel.TILE_DIM, MatrixKernel.TILE_DIM, 1)
  val gridSize = ((ROWS - 1) / MatrixKernel.TILE_DIM + 1, (ROWS - 1) / MatrixKernel.TILE_DIM + 1, 1)

  val deviceInputA = new CUdeviceptr
  JCudaDriver.cuMemAlloc(deviceInputA, ROWS * COLUMNS * Sizeof.FLOAT)
  JCudaDriver.cuMemcpyHtoD(deviceInputA, Pointer.to(aData), ROWS * COLUMNS * Sizeof.FLOAT)

  val deviceInputB = new CUdeviceptr
  JCudaDriver.cuMemAlloc(deviceInputB, COLUMNS * ROWS * Sizeof.FLOAT)
  JCudaDriver.cuMemcpyHtoD(deviceInputB, Pointer.to(bData), COLUMNS * ROWS * Sizeof.FLOAT)

  val deviceOutputC = new CUdeviceptr
  JCudaDriver.cuMemAlloc(deviceOutputC, ROWS * ROWS * Sizeof.FLOAT)

  val kernelParams = Pointer.to(
    Pointer.to(deviceInputA), Pointer.to(deviceInputB), Pointer.to(deviceOutputC),
    Pointer.to(Array[Int](ROWS)), Pointer.to(Array[Int](COLUMNS)),
    Pointer.to(Array[Int](COLUMNS)), Pointer.to(Array[Int](ROWS)),
    Pointer.to(Array[Int](ROWS)), Pointer.to(Array[Int](ROWS))
  )

  val multiplyTime = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      kernelParams, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"raw matrix * matrix time: $multiplyTime")
}

object MatrixTranspose extends App with MatrixPerformance {
  //base: matrix transpose time: 5.22756384 ms
  //tiled: matrix transpose time: 4.736962640000002 ms
  //tiled: matrix transpose time: 0.9630467999999998 ms
  val a = Matrix.of(NativeMatrix.matrixData[Float](ROWS, COLUMNS))

  val multiplyTime = standardConfig measure {
    val result = a.t
  }

  println(s"matrix transpose time: $multiplyTime")
}
