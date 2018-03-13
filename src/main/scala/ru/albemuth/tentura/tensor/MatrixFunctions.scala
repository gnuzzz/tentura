package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.{MatrixSum, MatrixSumColumns, MatrixSumRows}
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.vector.VectorKernel

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object MatrixFunctions {

  def sum[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    ScalarKernel.scalar_r(matrixSum, matrix, result)
  }

  def sum[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    ScalarKernel.scalar(matrixSum, matrix)
  }

  def sum[T: ClassTag](matrix: Matrix[T], axis: Int): Vector[T] = {
    if (axis == 0) {
      sumRows(matrix)
    } else {
      sumColumns(matrix)
    }
  }

  def sum[T: ClassTag](matrix: Matrix[T], axis: Int, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      sumRows(matrix, result)
    } else {
      sumColumns(matrix, result)
    }
  }

  def sumRows[T: ClassTag](matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixSumRows, matrix, result)
  }

  def sumRows[T: ClassTag](matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(matrixSumRows, matrix, new Vector[T](matrix.columns))
  }

  def sumColumns[T: ClassTag](matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(matrixSumColumns, matrix, result)
  }

  def sumColumns[T: ClassTag](matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(matrixSumColumns, matrix, new Vector[T](matrix.rows))
  }

  lazy val matrixSum = new KernelTemplate(new MatrixSum)
  lazy val matrixSumRows = new KernelTemplate(new MatrixSumRows)
  lazy val matrixSumColumns = new KernelTemplate(new MatrixSumColumns)

}
