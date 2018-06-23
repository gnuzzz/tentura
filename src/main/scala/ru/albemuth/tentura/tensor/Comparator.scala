package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object Comparator extends Enumeration {
  type Comparator = Value
  val == = Value(0)
  val != = Value(1)
  val < = Value(2)
  val <= = Value(3)
  val > = Value(4)
  val >= = Value(5)
}
