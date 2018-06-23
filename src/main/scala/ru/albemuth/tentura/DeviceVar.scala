package ru.albemuth.tentura

import jcuda.driver.{CUdeviceptr, JCudaDriver}
import ru.albemuth.tentura.kernel.JCudaKernel.{pointer, sizeOf}
import ru.albemuth.tentura.tensor.{Matrix, Scalar, Vector}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class DeviceVar[T: ClassTag] {

  protected val deviceDataPtr: CUdeviceptr
  protected val resultsCache = new ResultsCache

  def itemSizeOf(): Int = sizeOf()

  def result[R: ClassTag](function: AnyRef, resultKey: Any, result: => Matrix[R]): Matrix[R] = {
    resultsCache.result[Matrix[R]](function, resultKey, result)
  }

  def result[R: ClassTag](function: AnyRef, resultKey: Any, result: => Vector[R]): Vector[R] = {
    resultsCache.result[Vector[R]](function, resultKey, result)
  }

  def result[R: ClassTag](function: AnyRef, resultKey: Any, result: => Scalar[R]): Scalar[R] = {
    resultsCache.result[Scalar[R]](function, resultKey, result)
  }

  def copy2device(data: Array[T]): Unit = {
    JCudaDriver.cuMemcpyHtoD(deviceDataPtr, pointer(data), data.length * sizeOf())
  }

  def copy2device(data: Array[T], dstOffset: Int, length: Int): Unit = {
    JCudaDriver.cuMemcpyHtoD(deviceDataPtr.withByteOffset(dstOffset * sizeOf()), pointer(data), length * sizeOf())
  }

  def copy2device[/*@specialized(Boolean, Byte, Char, Short, Int, Long, Float, Double)*/ V <: T: ClassTag](value: V): Unit = {
    JCudaDriver.cuMemcpyHtoD(deviceDataPtr, pointer(Array(value)), sizeOf[V]())
  }

  def copy2device(value: T, dstOffset: Int): Unit = {
    JCudaDriver.cuMemcpyHtoD(deviceDataPtr.withByteOffset(dstOffset * sizeOf()), pointer(Array(value)), sizeOf())
  }

  def copy(src: DeviceVar[T], length: Int): Unit = {
    JCudaDriver.cuMemcpy(deviceDataPtr, src.deviceDataPtr, length * sizeOf())
  }

  def copy(src: DeviceVar[T], srcOffset: Int, dstOffset: Int, length: Int): Unit = {
    JCudaDriver.cuMemcpy(deviceDataPtr.withByteOffset(dstOffset * sizeOf()), src.deviceDataPtr.withByteOffset(srcOffset * sizeOf()), length * sizeOf())
  }

  def copy2host(data: Array[T]): Unit = {
    val dataPointer = pointer(data)
    JCudaDriver.cuMemcpyDtoH(dataPointer, deviceDataPtr, data.length * sizeOf())
    if (data.getClass.getComponentType == classOf[Boolean]) {
      val bytes = dataPointer.getByteBuffer(0, data.length).array()
      for (i <- data.indices) {
        data(i) = (bytes(i) > 0).asInstanceOf[T]
      }
    }
  }

  def release(): Unit = {
    JCudaDriver.cuMemFree(deviceDataPtr)
    //todo - free results in caches
  }

}
