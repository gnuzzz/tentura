package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object Operator extends Enumeration {
  type Operator = Value
  val + = Value(0)
  val - = Value(1)
  val * = Value(2)
  val / = Value(3)
  val ASSIGN = Value(-1)
}
