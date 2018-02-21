package ru.albemuth.tentura.kernel

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait Template[K <: GenericKernel] {

  def functionName: String

  def materialize(functionImplName: String): K

}
