package ru.albemuth.tentura

import jcuda.Pointer
import jcuda.driver.{CUdeviceptr, JCudaDriver}
import ru.albemuth.tentura.kernel.JCudaKernel.sizeOfItem

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait DeviceVar[T] {

  protected val data: Array[T]
  protected val dataPointer: Pointer
  protected val deviceDataPtr: CUdeviceptr
  protected val resultsCache = new ResultsCache

  def copy2device(): Unit = {
    JCudaDriver.cuMemcpyHtoD(deviceDataPtr, dataPointer, data.length * sizeOfItem(data))
  }

  def copy2host(): Unit = {
    JCudaDriver.cuMemcpyDtoH(dataPointer, deviceDataPtr, data.length * sizeOfItem(data))
  }

  def release(): Unit = {
    JCudaDriver.cuMemFree(deviceDataPtr)
    //todo - free results in caches
  }

}
