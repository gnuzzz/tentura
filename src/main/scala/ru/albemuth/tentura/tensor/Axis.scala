package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
case class Axis(value: Int) {
  def ==(v: Int): Boolean = value == v
}

object Axis {

  def apply(value: Int): Axis = new Axis(value)

  def of (value: Int): Axis = new Axis(value)

  implicit def toInt(axis: Axis): Int = axis.value

  implicit def fromInt(value: Int): Axis = new Axis(value)

}