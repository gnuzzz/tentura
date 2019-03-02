package ru.albemuth.tentura.sort

import java.util

import ru.albemuth.jcuda.jcusegsort.{Datatype, Sorting}
import ru.albemuth.tentura.tensor.{NativeVector, Vector}


/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object TestSort extends App {

  def assertArrayEquals(a1: Array[Float], a2: Array[Float]): Unit = {
    if (!(a1.size == a2.size && a1.zip(a2).forall(p => p._1 == p._2))) throw new IllegalStateException("arrays not equal")
  }

  val N = 40000000
  val data = NativeVector.vectorData(N)
  val keys = Vector.of(data)
//  Sorting.sort(keys.deviceDataPtr, Datatype.FLOAT, keys.length)
//  val sortedKeys = keys.values()
  val sortedKeys = Vector.sort(keys)
  val hostSortedKeys = data.clone
  util.Arrays.sort(hostSortedKeys)
//  assertArrayEquals(sortedKeys, hostSortedKeys)
  assertArrayEquals(sortedKeys.values(), hostSortedKeys)

/*
  val n = 10
  val data: Array[Int] = Array.fill(n)((Math.random() * 100).toInt)
//  val data: Array[Long] = Array.fill(n)((Math.random() * 100).toLong) //not works
//  val data: Array[Float] = Array.fill(n)((Math.random() * 100).toFloat)
//  val data: Array[Double] = Array.fill(n)(Math.random() * 100) //not works
  data.foreach(println(_))
  println("----------")

  val dataPointer = JCudaKernel.pointer(data)

  // Allocate memory on the device
  val deviceDataPtr = JCudaKernel.devicePtr(data.length)

  // Copy the input array from the host to the device
  JCudaDriver.cuMemcpyHtoD(deviceDataPtr, dataPointer, data.length * sizeOf[Int]())

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
  JCudaDriver.cuMemcpyDtoH(dataPointer, deviceDataPtr, data.length * sizeOf[Int]())
  JCuda.cudaMemcpy(Pointer.to(data), deviceDataPtr, n * Sizeof.INT, cudaMemcpyKind.cudaMemcpyDeviceToHost)

  // Clean up
  JCudpp.cudppDestroyPlan(handle)
  JCudpp.cudppDestroy(theCudpp)
  JCuda.cudaFree(deviceDataPtr)

  data.foreach(println(_))
  */
}
