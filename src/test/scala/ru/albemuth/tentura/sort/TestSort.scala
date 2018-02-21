package ru.albemuth.tentura.sort

import jcuda.{Pointer, Sizeof}
import jcuda.driver.JCudaDriver
import jcuda.jcudpp._
import jcuda.runtime.{JCuda, cudaMemcpyKind}
import ru.albemuth.tentura.kernel.JCudaKernel
import ru.albemuth.tentura.kernel.JCudaKernel.sizeOfItem


/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object TestSort extends App {

  val n = 10
  val data: Array[Int] = Array.fill(n)((Math.random() * 100).toInt)
//  val data: Array[Long] = Array.fill(n)((Math.random() * 100).toLong) //not works
//  val data: Array[Float] = Array.fill(n)((Math.random() * 100).toFloat)
//  val data: Array[Double] = Array.fill(n)(Math.random() * 100) //not works
  data.foreach(println(_))
  println("----------")

  val dataPointer = JCudaKernel.pointer(data)

  // Allocate memory on the device
  val deviceDataPtr = JCudaKernel.devicePtr(data)

  // Copy the input array from the host to the device
  JCudaDriver.cuMemcpyHtoD(deviceDataPtr, dataPointer, data.length * sizeOfItem(data))

  // Create a CUDPPConfiguration for a radix sort of an array
  val config = new CUDPPConfiguration
//  config.algorithm = CUDPPAlgorithm.CUDPP_SORT_RADIX
  config.algorithm = CUDPPAlgorithm.CUDPP_SORT_MERGE
  config.datatype = CUDPPDatatype.CUDPP_UINT
  config.datatype = CUDPPDatatype.CUDPP_INT
//  config.op = CUDPPOperator.CUDPP_ADD
  config.options = CUDPPOption.CUDPP_OPTION_KEYS_ONLY

  // Create a CUDPPHandle for the sort operation
  val theCudpp = new CUDPPHandle
  JCudpp.cudppCreate(theCudpp)
  val handle = new CUDPPHandle
  JCudpp.cudppPlan(theCudpp, handle, config, n, 1, 0)

  // Execute the sort operation
  val t1 = System.nanoTime
//  JCudpp.cudppRadixSort(handle, deviceDataPtr, null, n)
  JCudpp.cudppMergeSort(handle, deviceDataPtr, null, n)
  val t2 = System.nanoTime

//  util.Arrays.fill(array, 0)

  // Copy the result from the device to the host
  JCudaDriver.cuMemcpyDtoH(dataPointer, deviceDataPtr, data.length * sizeOfItem(data))
  JCuda.cudaMemcpy(Pointer.to(data), deviceDataPtr, n * Sizeof.INT, cudaMemcpyKind.cudaMemcpyDeviceToHost)

  // Clean up
  JCudpp.cudppDestroyPlan(handle)
  JCudpp.cudppDestroy(theCudpp)
  JCuda.cudaFree(deviceDataPtr)

  data.foreach(println(_))
}
