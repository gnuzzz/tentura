package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.kernel.vector._
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel.{scalar, scalar_r}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object VectorFunctions {

  def bincount(vector: Vector[Int]): Vector[Int] = {
    val maxValue = max(vector).value()
    bincount(vector, maxValue)
  }

  def bincount(vector: Vector[Int], maxValue: Int, result: Vector[Int]): Vector[Int] = {
    VectorKernel.vector_r(vectorBincount, vector, maxValue, result)
  }

  def bincount(vector: Vector[Int], maxValue: Int): Vector[Int] = {
    val result = vector.result(vectorBincount, maxValue, new Vector[Int](maxValue + 1))
    VectorKernel.vector_r(vectorBincount, vector, maxValue, result)
  }

  def sort[T: ClassTag](vector: Vector[T]): (Vector[T], SortOperation) = {
    val sort = CudppSort.sort(vector)
    val result = sort(vector)
    (result, sort)
  }

  def sort[T: ClassTag](vector: Vector[T], order: SortOrder): (Vector[T], SortOperation) = {
    val sort = CudppSort.sort(vector, order)
    val result = sort(vector)
    (result, sort)
  }

  def argsort[T: ClassTag](vector: Vector[T]): (Vector[Int], ArgsortOperation) = {
    val argsort = CudppSort.argsort(vector)
    val result = argsort(vector)
    (result, argsort)
  }

  def argsort[T: ClassTag](vector: Vector[T], order: SortOrder): (Vector[Int], ArgsortOperation) = {
    val argsort = CudppSort.argsort(vector, order)
    val result = argsort(vector)
    (result, argsort)
  }

  def sum[T: ClassTag](vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(vectorSum, vector, result)
  }

  def sum[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(vectorSum, vector)
  }

  def max[T: ClassTag](vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(vectorMax, vector, result)
  }

  def max[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(vectorMax, vector)
  }

  def min[T: ClassTag](vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(vectorMin, vector, result)
  }

  def min[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(vectorMin, vector)
  }

  def argmax[T: ClassTag](vector: Vector[T], result: Scalar[Int]): Scalar[Int] = {
    scalar_r(vectorArgmax, vector, result)
  }

  def argmax[T: ClassTag](vector: Vector[T]): Scalar[Int] = {
    scalar(vectorArgmax, vector)
  }

  def argmin[T: ClassTag](vector: Vector[T], result: Scalar[Int]): Scalar[Int] = {
    scalar_r(vectorArgmin, vector, result)
  }

  def argmin[T: ClassTag](vector: Vector[T]): Scalar[Int] = {
    scalar(vectorArgmin, vector)
  }

  lazy val vectorBincount = new VectorBincount
  lazy val vectorSum = new KernelTemplate(new VectorSum)
  lazy val vectorMax = new KernelTemplate(new VectorMax)
  lazy val vectorMin = new KernelTemplate(new VectorMin)
  lazy val vectorArgmax = new KernelTemplate(new VectorArgmax)
  lazy val vectorArgmin = new KernelTemplate(new VectorArgmin)

}
