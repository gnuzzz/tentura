package ru.albemuth.tentura.util

import jcuda.driver.JCudaDriver
import ru.albemuth.tentura.kernel.JCudaKernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object Memory {

  private val num_sm = JCudaKernel.numberOfSMs

  def check[R](s: String)(block: => R): R = {
    val (total0, free0) = memory()
    val result = block
    val (tota10, free1) = memory()
    if (free1 != free0) println(s"$s: ${free0 - free1}")
    result
  }

  def print(s: String): Unit = {
    val (total, free) = memory()
    println(s"$s: $total/$free")
  }

  def memory(): (Long, Long) = {
    val total = Array.ofDim[Long](1)
    val free = Array.ofDim[Long](1)
    JCudaDriver.cuMemGetInfo(free, total)
    (total(0), free(0))
  }

}
