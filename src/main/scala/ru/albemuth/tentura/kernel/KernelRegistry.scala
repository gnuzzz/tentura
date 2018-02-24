package ru.albemuth.tentura.kernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object KernelRegistry {

  def classifier[K <: JCudaKernel](kernelClass: Class[K]): String = "" //todo

}
