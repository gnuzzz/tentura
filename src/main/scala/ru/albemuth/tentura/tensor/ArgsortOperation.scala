package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait ArgsortOperation extends Operation {

  def apply[T, R](vector: Vector[T], args: Vector[R]): Vector[R]

  def apply[T](vector: Vector[T]): Vector[Int] = {
    val result: Vector[Int] = vector.result(this, vector, new Vector[Int](vector.length))
    result.copy2device(Array.tabulate(vector.length)(i => i))
    apply(vector, result)
  }

}
