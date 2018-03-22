package ru.albemuth.tentura.tensor

import jcuda.Pointer
import jcuda.driver.JCudaDriver
import org.scalatest.FunSuiteLike
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.performance.MatrixPerformance
import ru.albemuth.tentura.tensor.MathFunctions.sigmoid

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class MatrixExp {
}

object MatrixAddMatrix extends App with MatrixPerformance {
  //matrix + matrix time: 6.076431480000001 ms
  val rows = 5120
  val columns = 1280
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val time = standardConfig measure {
    val result = a + b
  }

  println(s"matrix + matrix time: $time")
}

object MatrixAddMatrixExp extends App with MatrixPerformance {
  //raw matrix + matrix time: 5.97475136 ms
  //raw matrix + matrix time: 2.37903732 ms
  val rows = 5120
  val columns = 1280
  val tileDim = 32
  val blockRows = 8

  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixPerformance", "", "matrixAddMatrix")
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val result = new Matrix[Float](rows, columns)

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = ((columns - 1) / tileDim + 1, (rows - 1) / tileDim + 1, 1)

  val kernelParams = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(b.deviceDataPtr), Pointer.to(result.deviceDataPtr),
    Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
  )

  val time = standardConfig measure {
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

  println(s"raw matrix + matrix time: $time")
}

object MatrixAddScalar extends App with MatrixPerformance {
  //matrix + scalar time: 2.93733568 ms
  val rows = 5120
  val columns = 1280
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = 11.2f
  val time = standardConfig measure {
    val result = a + b
  }

  println(s"matrix + scalar time: $time")
}

object MatrixAddScalarExp extends App with MatrixPerformance {
  //raw matrix + scalar time: 2.9024464800000005 ms
  //raw matrix + scalar time: 2.4310311199999997 ms: 16, 8
  //raw matrix + scalar time: 3.3428522000000003 ms: 16, 4
  //raw matrix + scalar time: 2.0632501199999997 ms: 32, 16
  //raw matrix + scalar time: 1.6534434399999998 ms: 32, 8
  //raw matrix + scalar time: 1.8990273999999996 ms: 32, 4
  val rows = 5120
  val columns = 1280
  val tileDim = 32
  val blockRows = 16

  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixPerformance", "", "matrixAddScalar")
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = 11.2f
  val result = new Matrix[Float](rows, columns)

//  val blockSize = (tileDim, tileDim, 1)
  val blockSize = (tileDim, blockRows, 1)
  val gridSize = ((columns - 1) / tileDim + 1, (rows - 1) / tileDim + 1, 1)

  val kernelParams = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(Array[Float](b)), Pointer.to(result.deviceDataPtr),
    Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
  )

  val time = standardConfig measure {
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

  println(s"raw matrix + scalar time: $time")
}

object MatrixSigmoid extends App with MatrixPerformance {
  //sigmoid(matrix) time: 3.7141296799999997 ms
  val rows = 5120
  val columns = 1280
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val time = standardConfig measure {
    val result = sigmoid(a)
  }

  println(s"sigmoid(matrix) time: $time")
}

object MatrixSigmoidExp extends App with MatrixPerformance {
  //raw sigmoid(matrix) time: 3.3660291600000005 ms: 16, 8
  //raw sigmoid(matrix) time: 4.941940239999998 ms: 16, 4
  //raw sigmoid(matrix) time: 3.1930833200000013 ms: 32, 16
  //raw sigmoid(matrix) time: 2.7792821200000004 ms: 32, 8
  //raw sigmoid(matrix) time: 2.950557039999999 ms: 32, 4
  val rows = 5120
  val columns = 1280
  val tileDim = 32
  val blockRows = 4

  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixPerformance", "", "matrixSigmoid")
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = 11.2f
  val result = new Matrix[Float](rows, columns)

  //  val blockSize = (tileDim, tileDim, 1)
  val blockSize = (tileDim, blockRows, 1)
  val gridSize = ((columns - 1) / tileDim + 1, (rows - 1) / tileDim + 1, 1)

  val kernelParams = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(result.deviceDataPtr),
    Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns))
  )

  val time = standardConfig measure {
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

  println(s"raw sigmoid(matrix) time: $time")
}

object MatrixMulMatrix extends App with MatrixPerformance {
  //matrix * matrix time: 1926.7138063200002 ms
  val rows = 5120
  val columns = 1280
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = Matrix.of(NativeMatrix.matrixData[Float](columns, rows))
  val time = standardConfig measure {
    val result = a * b
  }

  println(s"matrix * matrix time: $time")
}

object MatrixMulMatrixExp extends App with MatrixPerformance with TestUtils with FunSuiteLike {
  //raw matrix * matrix time: 1271.4413795599999 ms: 16, 8     //224
  //raw matrix * matrix time: 1037.43175124 ms: 16, 4          //197
  //raw matrix * matrix time:                 : 32, 32         //235
  //raw matrix * matrix time: 1108.4955965200002 ms: 32, 16    //205
  //raw matrix * matrix time: 878.7206756800002 ms: 32, 8      //152
  //raw matrix * matrix time: 771.4476948 ms: 32, 4            //131
  //raw matrix * matrix time: 888.2055816 ms: 32, 2            //136
  //raw matrix * matrix time:               : 64, 8            //143
  //raw matrix * matrix time:               : 64, 4            //125
  //raw matrix * matrix time:               : 64, 4 without +1 //96
  //raw matrix * matrix time:               : 64, 2            //166
  val rows = 5120
//  val rows = 512
  val columns = 1280
//  val columns = 128
  val tileDim = 64
  val blockRows = 4

  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixPerformance", "", "matrixMulMatrix")
//  val kernel = JCudaKernel.loadKernel("/ru/albemuth/tentura/tensor/kernel/matrix/Matrix.cu", "matrixMulMatrix")
  val a = Matrix.of(NativeMatrix.matrixData[Float](rows, columns))
  val b = Matrix.of(NativeMatrix.matrixData[Float](columns, rows))
  val result = new Matrix[Float](rows, rows)

//    val blockSize = (tileDim, tileDim, 1)
  val blockSize = (tileDim, blockRows, 1)
  val gridSize = ((rows - 1) / tileDim + 1, (rows - 1) / tileDim + 1, 1)

  val kernelParams = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(b.deviceDataPtr), Pointer.to(result.deviceDataPtr),
    Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](columns)),
    Pointer.to(Array[Int](columns)), Pointer.to(Array[Int](rows)),
    Pointer.to(Array[Int](rows)), Pointer.to(Array[Int](rows))
  )

//  JCudaDriver.cuLaunchKernel(
//    kernel,
//    gridSize._1, gridSize._2, gridSize._3,
//    blockSize._1, blockSize._2, blockSize._3,
//    //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
//    0, null, // Shared memory size and stream
//    kernelParams, null // Kernel- and extra parameters
//  )
//  JCudaDriver.cuCtxSynchronize
//  result.copy2host()
//  val nativeA = new NativeMatrix(a.values())
//  val nativeB = new NativeMatrix(b.values())
//  val nativeResult = nativeA * nativeB
//  val maxError = compare(result.values(), nativeResult.data)
////  printMatrix(a.values())
////  println("---")
////  printMatrix(b.values())
////  println("===")
////  printMatrix(result.values())
////  println("---")
////  printMatrix(nativeResult.data)
//  assert(maxError < 0.0001)

  val time = standardConfig measure {
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

  println(s"raw matrix * matrix time: $time")
}