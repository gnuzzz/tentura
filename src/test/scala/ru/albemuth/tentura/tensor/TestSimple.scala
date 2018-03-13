package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestSimple extends FunSuite {

  val ROWS = 512
  val COLUMNS = 128

  test("test") {
    val a = Matrix.of[Float](NativeMatrix.matrixData(ROWS, COLUMNS))
    val b = new Vector[Float](NativeVector.vectorData(COLUMNS))

    for (i <- 0 until 10) {
      val result = a * b
      println(result)
    }
  }

  test("v") {
    val v1 = V1.of(Array(1, 2, 3))
    val v2 = V1.of(Array(4, 5, 6))
    val r1 = v1.plus(v2)
    val v: V1[Int] = V1.of(Array(7, 8, 9))
    val r = v1 + v2
    r.asInstanceOf[VImpl[Int]].data.foreach(println(_))
  }

  test("aaa") {
    val c1 = new C(1)
    println(c1.plus(1).value())
  }

}

class C[T: ClassTag](val v: Int) {

  val data = Array(v)

  def plus(scalar: Int): C[T] = {
    new C(data(0) + scalar)
  }

  def value(): Int = data(0)

}

abstract class V[T, -PARAM <: V[T, _, _], +RESULT <: V[T, _, _]] {

  def apply(i: Int): T

  def +(p: PARAM): RESULT

  def plus(p: PARAM): RESULT

}

trait V1[T] {

  def +(p: V1[T]): V1[T]

  def plus(p: V1[T]): V1[T]

}

object V1 {
  def of[T: ClassTag](data: Array[T]): V1[T] = {
    new VImpl[T](data)
  }
}

class VImpl[T: ClassTag](val data: Array[T]) extends V[T, VImpl[T], VImpl[T]] with V1[T] {

  override def +(p: V1[T]): V1[T] = this + p.asInstanceOf[VImpl[T]]

  override def plus(p: V1[T]): V1[T] = plus(p.asInstanceOf[VImpl[T]])

  override def apply(i: Int): T = data(i)

  override def +(p: VImpl[T]): VImpl[T] = {
    val result = for (i <- data.indices) yield {
      sum(data(i), p.data(i))
    }
    new VImpl(result.toArray)
  }

  override def plus(p: VImpl[T]): VImpl[T] = this + p

  private def sum(t1: T, t2: T): T = {
    data.getClass.getComponentType match {
      case b if b == classOf[Boolean] => ???
      case b if b == classOf[Byte] => (t1.asInstanceOf[Byte] + t2.asInstanceOf[Byte]).asInstanceOf[T]
      case c if c == classOf[Char] => (t1.asInstanceOf[Char] + t2.asInstanceOf[Char]).asInstanceOf[T]
      case s if s == classOf[Short] => (t1.asInstanceOf[Short] + t2.asInstanceOf[Short]).asInstanceOf[T]
      case i if i == classOf[Int] => (t1.asInstanceOf[Int] + t2.asInstanceOf[Int]).asInstanceOf[T]
      case l if l == classOf[Long] => (t1.asInstanceOf[Long] + t2.asInstanceOf[Long]).asInstanceOf[T]
      case f if f == classOf[Float] => (t1.asInstanceOf[Float] + t2.asInstanceOf[Float]).asInstanceOf[T]
      case d if d == classOf[Double] => (t1.asInstanceOf[Double] + t2.asInstanceOf[Double]).asInstanceOf[T]
      case _ => ??? //not supported
    }
  }

}