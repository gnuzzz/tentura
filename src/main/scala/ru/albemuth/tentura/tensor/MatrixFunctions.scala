package ru.albemuth.tentura.tensor

import ru.albemuth.jcuda.jcusegsort.{KeySortContext, KeyValueSortContext, Sorting}
import ru.albemuth.tentura.kernel.JCudaKernel.datatype
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.kernel.matrix._
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel.{scalar, scalar_r}
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

  def min[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(matrixMin, matrix, result)
  }

  def min[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    scalar(matrixMin, matrix)
  }

  def min[T: ClassTag](matrix: Matrix[T], axis: Int, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixMinColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixMinColumn, matrix, result)
    }
  }

  def min[T: ClassTag](matrix: Matrix[T], axis: Int): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector(matrixMinColumn, matrix.T, new Vector[T](matrix.columns))
    } else {
      VectorKernel.vector(matrixMinColumn, matrix, new Vector[T](matrix.rows))
    }
  }

  def max[T: ClassTag](matrix: Matrix[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(matrixMax, matrix, result)
  }

  def max[T: ClassTag](matrix: Matrix[T]): Scalar[T] = {
    scalar(matrixMax, matrix)
  }

  def max[T: ClassTag](matrix: Matrix[T], axis: Int, result: Vector[T]): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixMaxColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixMaxColumn, matrix, result)
    }
  }

  def max[T: ClassTag](matrix: Matrix[T], axis: Int): Vector[T] = {
    if (axis == 0) {
      VectorKernel.vector(matrixMaxColumn, matrix.T, new Vector[T](matrix.columns))
    } else {
      VectorKernel.vector(matrixMaxColumn, matrix, new Vector[T](matrix.rows))
    }
  }

  def argmin[T: ClassTag](matrix: Matrix[T], result: Vector[Int]): Vector[Int] = {
    argmin(matrix, axis = 1, result)
  }

  def argmin[T: ClassTag](matrix: Matrix[T]): Vector[Int] = {
    argmin(matrix, axis = 1)
  }

  def argmin[T: ClassTag](matrix: Matrix[T], axis: Int, result: Vector[Int]): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixArgminColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixArgminColumn, matrix, result)
    }
  }

  def argmin[T: ClassTag](matrix: Matrix[T], axis: Int): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector(matrixArgminColumn, matrix.T, new Vector[Int](matrix.columns))
    } else {
      VectorKernel.vector(matrixArgminColumn, matrix, new Vector[Int](matrix.rows))
    }
  }

  def argmax[T: ClassTag](matrix: Matrix[T], result: Vector[Int]): Vector[Int] = {
    argmax(matrix, axis = 1, result)
  }

  def argmax[T: ClassTag](matrix: Matrix[T]): Vector[Int] = {
    argmax(matrix, axis = 1)
  }

  def argmax[T: ClassTag](matrix: Matrix[T], axis: Int, result: Vector[Int]): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector_r(matrixArgmaxColumn, matrix.T, result)
    } else {
      VectorKernel.vector_r(matrixArgmaxColumn, matrix, result)
    }
  }

  def argmax[T: ClassTag](matrix: Matrix[T], axis: Int): Vector[Int] = {
    if (axis == 0) {
      VectorKernel.vector(matrixArgmaxColumn, matrix.T, new Vector[Int](matrix.columns))
    } else {
      VectorKernel.vector(matrixArgmaxColumn, matrix, new Vector[Int](matrix.rows))
    }
  }

  def sort[T: ClassTag](matrix: Matrix[T], context: KeySortContext): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns, context)
    matrix
  }

  def sort[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns)
    matrix
  }

  def sort[T: ClassTag](matrix: Matrix[T], order: SortOrder, context: KeySortContext): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns, context)
    if (order == SortOrder.ASC) {
      matrix
    } else {
      matrix.reverse(1)
    }
  }

  def sort[T: ClassTag](matrix: Matrix[T], order: SortOrder): Matrix[T] = {
    Sorting.sort(matrix.deviceDataPtr, datatype(), matrix.rows, matrix.columns)
    if (order == SortOrder.ASC) {
      matrix
    } else {
      matrix.reverse(1)
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], context: KeyValueSortContext): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns, context)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V]): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], order: SortOrder, context: KeyValueSortContext): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns, context)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(1), values.reverse(1))
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Matrix[K], values: Matrix[V], order: SortOrder): (Matrix[K], Matrix[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.rows, keys.columns)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(1), values.reverse(1))
    }
  }

  def argsort[T: ClassTag](matrix: Matrix[T], context: KeyValueSortContext): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, context)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T]): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T], order: SortOrder, context: KeyValueSortContext): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, order, context)
    indices
  }

  def argsort[T: ClassTag](matrix: Matrix[T], order: SortOrder): Matrix[Int] = {
    val indices = matrix.indices(1)
    sort(matrix, indices, order)
    indices
  }

  def bincount(matrix: Matrix[Int], axis: Int): Matrix[Int] = {
    val maxValue = max(matrix).value()
    bincount(matrix, maxValue, axis)
  }

  def bincount(matrix: Matrix[Int], maxValue: Int, axis: Int, result: Matrix[Int]): Matrix[Int] = {
    if (axis == 0) {
      MatrixKernel.matrix_r(matrixBincount, matrix.T, maxValue, result).T
    } else {
      MatrixKernel.matrix_r(matrixBincount, matrix, maxValue, result)
    }
  }

  def bincount(matrix: Matrix[Int], maxValue: Int, axis: Int): Matrix[Int] = {
    if (axis == 0) {
      MatrixKernel.matrix(matrixBincount, matrix.T, maxValue, new Matrix[Int](matrix.columns, maxValue + 1)).T
    } else {
      MatrixKernel.matrix(matrixBincount, matrix, maxValue, new Matrix[Int](matrix.rows, maxValue + 1))
    }
  }

  lazy val matrixSum = new KernelTemplate(new MatrixSum)
  lazy val matrixSumRows = new KernelTemplate(new MatrixSumRows)
  lazy val matrixSumColumns = new KernelTemplate(new MatrixSumColumns)
  lazy val matrixMin = new KernelTemplate(new MatrixMin)
  lazy val matrixMax = new KernelTemplate(new MatrixMax)
  lazy val matrixMinColumn = new KernelTemplate(new MatrixMinColumn)
  lazy val matrixMaxColumn = new KernelTemplate(new MatrixMaxColumn())
  lazy val matrixArgminColumn = new KernelTemplate(new MatrixArgminColumn)
  lazy val matrixArgmaxColumn = new KernelTemplate(new MatrixArgmaxColumn)
  lazy val matrixBincount = new MatrixBincount

}
