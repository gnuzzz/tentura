package ru.albemuth.tentura.kernel

import scala.reflect.ClassTag
import KernelTemplate.functionName

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class KernelTemplate[K <: GenericKernel](template: Template[K]) {

  lazy val booleanKernel: K = template.materialize(functionName[Boolean](template.functionName))
  lazy val byteKernel: K = template.materialize(functionName[Byte](template.functionName))
  lazy val charKernel: K = template.materialize(functionName[Char](template.functionName))
  lazy val shortKernel: K = template.materialize(functionName[Short](template.functionName))
  lazy val intKernel: K = template.materialize(functionName[Int](template.functionName))
  lazy val longKernel: K = template.materialize(functionName[Long](template.functionName))
  lazy val floatKernel: K = template.materialize(functionName[Float](template.functionName))
  lazy val doubleKernel: K = template.materialize(functionName[Double](template.functionName))

  def kernel[T: ClassTag]: K = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => booleanKernel
      case b if b == classOf[Byte] => byteKernel
      case c if c == classOf[Char] => charKernel
      case s if s == classOf[Short] => shortKernel
      case i if i == classOf[Int] => intKernel
      case l if l == classOf[Long] => longKernel
      case f if f == classOf[Float] => floatKernel
      case d if d == classOf[Double] => doubleKernel
      case _ => ???
    }
  }

}

object KernelTemplate {

  def functionName[T: ClassTag](functionName: String): String = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => functionName + "_Boolean"
      case b if b == classOf[Byte] => functionName + "_Byte"
      case c if c == classOf[Char] => functionName + "_Char"
      case s if s == classOf[Short] => functionName + "_Short"
      case i if i == classOf[Int] => functionName + "_Int"
      case l if l == classOf[Long] => functionName + "_Long"
      case f if f == classOf[Float] => functionName + "_Float"
      case d if d == classOf[Double] => functionName + "_Double"
      case _ => functionName
    }
  }

}
