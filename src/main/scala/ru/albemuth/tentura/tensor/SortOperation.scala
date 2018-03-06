package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait SortOperation extends Operation {

  def apply[T](vector: Vector[T]): Vector[T]

}
