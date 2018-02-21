package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixSum
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel.scalar

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object MatrixFunctions {

  def sum[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    scalar(matrixSum, matrix)
  }

  lazy val matrixSum = new KernelTemplate(new MatrixSum)

}
