package ru.albemuth.tentura.tensor

import jcuda.Pointer
import jcuda.driver.JCudaDriver
import org.scalatest.FunSuiteLike
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.performance.MatrixPerformance

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TransposePerformance {

}

object MatrixCopy extends App with MatrixPerformance {
  //matrix copy time: 0.46422063999999996 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "copy")
  val width = 5073
  val tileDim = 64
  val blockRows = 4
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(result.deviceDataPtr), Pointer.to(a.deviceDataPtr)
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix copy time: $time")
}

object MatrixTransposeNaive extends App with MatrixPerformance {
  //matrix transposeNaive time: 1.6034827600000006 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeNaive")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(result.deviceDataPtr), Pointer.to(a.deviceDataPtr)
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeNaive time: $time")
}

object MatrixTransposeCoalesced extends App with MatrixPerformance {
  //matrix transposeCoalesced time: 0.8344074 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeCoalesced")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(result.deviceDataPtr), Pointer.to(a.deviceDataPtr)
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeCoalesced time: $time")
}

object MatrixCopySharedMem extends App with MatrixPerformance {
  //matrix copySharedMem time: 0.39840684 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "copySharedMem")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(result.deviceDataPtr), Pointer.to(a.deviceDataPtr)
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix copySharedMem time: $time")
}

object MatrixTransposeNoBankConflicts extends App with MatrixPerformance {
  //matrix transposeNoBankConflicts time: 0.42154356 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeNoBankConflicts")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(result.deviceDataPtr), Pointer.to(a.deviceDataPtr)
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeNoBankConflicts time: $time")
}

object MatrixTranspose extends App with MatrixPerformance {
  //matrix transpose time: 0.4132885600000001 ms
  //matrix transpose time: 0.4422022800000001 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transpose")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)



  val time = standardConfig measure {
    val params = Pointer.to(
      Pointer.to(a.deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](width)), Pointer.to(Array[Int](width))
    )

    val blockSize = (tileDim, blockRows, 1)
    val gridSize = (width / tileDim, width / tileDim, 1)

    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transpose time: $time")
}

object MatrixTransposeDouble extends App with MatrixPerformance {
  //matrix transposeDouble time: 0.7591186799999999 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeDouble")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Double](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Double](width, width)

  val params = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](width)), Pointer.to(Array[Int](width))
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeDouble time: $time")
}

object MatrixTransposeTemplate extends App with MatrixPerformance {
  //matrix transposeTemplateFloat time: 0.404422 ms
  //matrix transposeTemplateDouble time: 0.7437388399999999 ms
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeTemplateFloat")
  //val kernel = JCudaKernel.loadKernel("/ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose.cu", "transposeTemplateDouble")
  val width = 1024
  val tileDim = 32
  val blockRows = 8
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  //val a = new Matrix[Double](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)
  //val result = new Matrix[Double](width, width)

  val params = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](width)), Pointer.to(Array[Int](width))
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = (width / tileDim, width / tileDim, 1)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeTemplateFloat time: $time")
  //println(s"matrix transposeTemplateDouble time: $time")
}

object MatrixTransposeExperimental extends App with MatrixPerformance with TestUtils with FunSuiteLike {
  //matrix transposeExperimental time: 0.9346931599999999 ms - single
  //matrix transposeExperimental time: 0.41052471999999995 ms
  //matrix transposeExperimental time: 0.5415987200000001 ms - real

  //matrix transposeExperimental time: 3.1955386399999997 ms  //32, 8
  //matrix transposeExperimental time: 3.1973792799999994 ms  //32, 4
  //matrix transposeExperimental time: 2.70111596 ms          //64, 8
  //matrix transposeExperimental time: 2.682935 ms            //64, 4
  val kernel = JCudaKernel.loadKernel("ru/albemuth/tentura/tensor/kernel/matrix/MatrixTranspose", "", "transposeExperimental")
  val width = 5073
//  val width = 2
  val tileDim = 64
  val blockRows = 4
  val a = Matrix.of[Float](NativeMatrix.matrixData(width, width))
  val result = new Matrix[Float](width, width)

  val params = Pointer.to(
    Pointer.to(a.deviceDataPtr), Pointer.to(result.deviceDataPtr), Pointer.to(Array[Int](width)), Pointer.to(Array[Int](width))
  )

  val blockSize = (tileDim, blockRows, 1)
  val gridSize = ((width - 1) / tileDim + 1, (width - 1) / tileDim + 1, 1)

//  JCudaDriver.cuLaunchKernel(
//    kernel,
//    gridSize._1, gridSize._2, gridSize._3,
//    blockSize._1, blockSize._2, blockSize._3,
//    //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
//    0, null, // Shared memory size and stream
//    params, null // Kernel- and extra parameters
//  )
//  JCudaDriver.cuCtxSynchronize
//  result.copy2host()
//  val nativeMatrix = new NativeMatrix(a.values())
//  val nativeResult = nativeMatrix.t
//  val maxError = compare(result.values(), nativeResult.data)
//  assert(maxError === 0.0f)

  val time = standardConfig measure {
    JCudaDriver.cuLaunchKernel(
      kernel,
      gridSize._1, gridSize._2, gridSize._3,
      blockSize._1, blockSize._2, blockSize._3,
      //        TILE_WIDTH * TILE_WIDTH * Sizeof.FLOAT, null, // Shared memory size and stream
      0, null, // Shared memory size and stream
      params, null // Kernel- and extra parameters
    )
    JCudaDriver.cuCtxSynchronize
  }

  println(s"matrix transposeExperimental time: $time")
}