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

  lazy val booleanBooleanKernel: K = template.materialize(functionName[Boolean, Boolean](template.functionName))
  lazy val booleanByteKernel: K = template.materialize(functionName[Boolean, Byte](template.functionName))
  lazy val booleanCharKernel: K = template.materialize(functionName[Boolean, Char](template.functionName))
  lazy val booleanShortKernel: K = template.materialize(functionName[Boolean, Short](template.functionName))
  lazy val booleanIntKernel: K = template.materialize(functionName[Boolean, Int](template.functionName))
  lazy val booleanLongKernel: K = template.materialize(functionName[Boolean, Long](template.functionName))
  lazy val booleanFloatKernel: K = template.materialize(functionName[Boolean, Float](template.functionName))
  lazy val booleanDoubleKernel: K = template.materialize(functionName[Boolean, Double](template.functionName))

  lazy val byteBooleanKernel: K = template.materialize(functionName[Byte, Boolean](template.functionName))
  lazy val byteByteKernel: K = template.materialize(functionName[Byte, Byte](template.functionName))
  lazy val byteCharKernel: K = template.materialize(functionName[Byte, Char](template.functionName))
  lazy val byteShortKernel: K = template.materialize(functionName[Byte, Short](template.functionName))
  lazy val byteIntKernel: K = template.materialize(functionName[Byte, Int](template.functionName))
  lazy val byteLongKernel: K = template.materialize(functionName[Byte, Long](template.functionName))
  lazy val byteFloatKernel: K = template.materialize(functionName[Byte, Float](template.functionName))
  lazy val byteDoubleKernel: K = template.materialize(functionName[Byte, Double](template.functionName))

  lazy val charBooleanKernel: K = template.materialize(functionName[Char, Boolean](template.functionName))
  lazy val charByteKernel: K = template.materialize(functionName[Char, Byte](template.functionName))
  lazy val charCharKernel: K = template.materialize(functionName[Char, Char](template.functionName))
  lazy val charShortKernel: K = template.materialize(functionName[Char, Short](template.functionName))
  lazy val charIntKernel: K = template.materialize(functionName[Char, Int](template.functionName))
  lazy val charLongKernel: K = template.materialize(functionName[Char, Long](template.functionName))
  lazy val charFloatKernel: K = template.materialize(functionName[Char, Float](template.functionName))
  lazy val charDoubleKernel: K = template.materialize(functionName[Char, Double](template.functionName))

  lazy val shortBooleanKernel: K = template.materialize(functionName[Short, Boolean](template.functionName))
  lazy val shortByteKernel: K = template.materialize(functionName[Short, Byte](template.functionName))
  lazy val shortCharKernel: K = template.materialize(functionName[Short, Char](template.functionName))
  lazy val shortShortKernel: K = template.materialize(functionName[Short, Short](template.functionName))
  lazy val shortIntKernel: K = template.materialize(functionName[Short, Int](template.functionName))
  lazy val shortLongKernel: K = template.materialize(functionName[Short, Long](template.functionName))
  lazy val shortFloatKernel: K = template.materialize(functionName[Short, Float](template.functionName))
  lazy val shortDoubleKernel: K = template.materialize(functionName[Short, Double](template.functionName))

  lazy val intBooleanKernel: K = template.materialize(functionName[Int, Boolean](template.functionName))
  lazy val intByteKernel: K = template.materialize(functionName[Int, Byte](template.functionName))
  lazy val intCharKernel: K = template.materialize(functionName[Int, Char](template.functionName))
  lazy val intShortKernel: K = template.materialize(functionName[Int, Short](template.functionName))
  lazy val intIntKernel: K = template.materialize(functionName[Int, Int](template.functionName))
  lazy val intLongKernel: K = template.materialize(functionName[Int, Long](template.functionName))
  lazy val intFloatKernel: K = template.materialize(functionName[Int, Float](template.functionName))
  lazy val intDoubleKernel: K = template.materialize(functionName[Int, Double](template.functionName))

  lazy val longBooleanKernel: K = template.materialize(functionName[Long, Boolean](template.functionName))
  lazy val longByteKernel: K = template.materialize(functionName[Long, Byte](template.functionName))
  lazy val longCharKernel: K = template.materialize(functionName[Long, Char](template.functionName))
  lazy val longShortKernel: K = template.materialize(functionName[Long, Short](template.functionName))
  lazy val longIntKernel: K = template.materialize(functionName[Long, Int](template.functionName))
  lazy val longLongKernel: K = template.materialize(functionName[Long, Long](template.functionName))
  lazy val longFloatKernel: K = template.materialize(functionName[Long, Float](template.functionName))
  lazy val longDoubleKernel: K = template.materialize(functionName[Long, Double](template.functionName))

  lazy val floatBooleanKernel: K = template.materialize(functionName[Float, Boolean](template.functionName))
  lazy val floatByteKernel: K = template.materialize(functionName[Float, Byte](template.functionName))
  lazy val floatCharKernel: K = template.materialize(functionName[Float, Char](template.functionName))
  lazy val floatShortKernel: K = template.materialize(functionName[Float, Short](template.functionName))
  lazy val floatIntKernel: K = template.materialize(functionName[Float, Int](template.functionName))
  lazy val floatLongKernel: K = template.materialize(functionName[Float, Long](template.functionName))
  lazy val floatFloatKernel: K = template.materialize(functionName[Float, Float](template.functionName))
  lazy val floatDoubleKernel: K = template.materialize(functionName[Float, Double](template.functionName))

  lazy val doubleBooleanKernel: K = template.materialize(functionName[Double, Boolean](template.functionName))
  lazy val doubleByteKernel: K = template.materialize(functionName[Double, Byte](template.functionName))
  lazy val doubleCharKernel: K = template.materialize(functionName[Double, Char](template.functionName))
  lazy val doubleShortKernel: K = template.materialize(functionName[Double, Short](template.functionName))
  lazy val doubleIntKernel: K = template.materialize(functionName[Double, Int](template.functionName))
  lazy val doubleLongKernel: K = template.materialize(functionName[Double, Long](template.functionName))
  lazy val doubleFloatKernel: K = template.materialize(functionName[Double, Float](template.functionName))
  lazy val doubleDoubleKernel: K = template.materialize(functionName[Double, Double](template.functionName))

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

  def kernel[T1: ClassTag, T2: ClassTag]: K = {
    val clazz1 = implicitly[ClassTag[T1]].runtimeClass
    val clazz2 = implicitly[ClassTag[T2]].runtimeClass
    clazz1 match {
      case b if b == classOf[Boolean] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => booleanBooleanKernel
          case bb if bb == classOf[Byte] => booleanByteKernel
          case cc if cc == classOf[Char] => booleanCharKernel
          case ss if ss == classOf[Short] => booleanShortKernel
          case ii if ii == classOf[Int] => booleanIntKernel
          case ll if ll == classOf[Long] => booleanLongKernel
          case ff if ff == classOf[Float] => booleanFloatKernel
          case dd if dd == classOf[Double] => booleanDoubleKernel
          case _ => ???
        }
      case b if b == classOf[Byte] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => byteBooleanKernel
          case bb if bb == classOf[Byte] => byteByteKernel
          case cc if cc == classOf[Char] => byteCharKernel
          case ss if ss == classOf[Short] => byteShortKernel
          case ii if ii == classOf[Int] => byteIntKernel
          case ll if ll == classOf[Long] => byteLongKernel
          case ff if ff == classOf[Float] => byteFloatKernel
          case dd if dd == classOf[Double] => byteDoubleKernel
          case _ => ???
        }
      case c if c == classOf[Char] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => charBooleanKernel
          case bb if bb == classOf[Byte] => charByteKernel
          case cc if cc == classOf[Char] => charCharKernel
          case ss if ss == classOf[Short] => charShortKernel
          case ii if ii == classOf[Int] => charIntKernel
          case ll if ll == classOf[Long] => charLongKernel
          case ff if ff == classOf[Float] => charFloatKernel
          case dd if dd == classOf[Double] => charDoubleKernel
          case _ => ???
        }
      case s if s == classOf[Short] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => shortBooleanKernel
          case bb if bb == classOf[Byte] => shortByteKernel
          case cc if cc == classOf[Char] => shortCharKernel
          case ss if ss == classOf[Short] => shortShortKernel
          case ii if ii == classOf[Int] => shortIntKernel
          case ll if ll == classOf[Long] => shortLongKernel
          case ff if ff == classOf[Float] => shortFloatKernel
          case dd if dd == classOf[Double] => shortDoubleKernel
          case _ => ???
        }
      case i if i == classOf[Int] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => intBooleanKernel
          case bb if bb == classOf[Byte] => intByteKernel
          case cc if cc == classOf[Char] => intCharKernel
          case ss if ss == classOf[Short] => intShortKernel
          case ii if ii == classOf[Int] => intIntKernel
          case ll if ll == classOf[Long] => intLongKernel
          case ff if ff == classOf[Float] => intFloatKernel
          case dd if dd == classOf[Double] => intDoubleKernel
          case _ => ???
        }
      case l if l == classOf[Long] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => longBooleanKernel
          case bb if bb == classOf[Byte] => longByteKernel
          case cc if cc == classOf[Char] => longCharKernel
          case ss if ss == classOf[Short] => longShortKernel
          case ii if ii == classOf[Int] => longIntKernel
          case ll if ll == classOf[Long] => longLongKernel
          case ff if ff == classOf[Float] => longFloatKernel
          case dd if dd == classOf[Double] => longDoubleKernel
          case _ => ???
        }
      case f if f == classOf[Float] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => floatBooleanKernel
          case bb if bb == classOf[Byte] => floatByteKernel
          case cc if cc == classOf[Char] => floatCharKernel
          case ss if ss == classOf[Short] => floatShortKernel
          case ii if ii == classOf[Int] => floatIntKernel
          case ll if ll == classOf[Long] => floatLongKernel
          case ff if ff == classOf[Float] => floatFloatKernel
          case dd if dd == classOf[Double] => floatDoubleKernel
          case _ => ???
        }
      case d if d == classOf[Double] =>
        clazz2 match {
          case bb if bb == classOf[Boolean] => doubleBooleanKernel
          case bb if bb == classOf[Byte] => doubleByteKernel
          case cc if cc == classOf[Char] => doubleCharKernel
          case ss if ss == classOf[Short] => doubleShortKernel
          case ii if ii == classOf[Int] => doubleIntKernel
          case ll if ll == classOf[Long] => doubleLongKernel
          case ff if ff == classOf[Float] => doubleFloatKernel
          case dd if dd == classOf[Double] => doubleDoubleKernel
          case _ => ???
        }
      case _ => ???
    }
  }

}

object KernelTemplate {

  def functionName[T: ClassTag](functionName: String): String = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    functionName + "_" + clazz.getName
  }

  def functionName[T1: ClassTag, T2: ClassTag](functionName: String): String = {
    val clazz1 = implicitly[ClassTag[T1]].runtimeClass
    val clazz2 = implicitly[ClassTag[T2]].runtimeClass
    functionName + "_" + clazz1.getName + "_" + clazz2.getName
  }

}
