package ru.albemuth.tentura.tensor

import ru.albemuth.jcuda.jcusegsort.{KeySortContext, KeyValueSortContext, Sorting}
import ru.albemuth.tentura.kernel.JCudaKernel.datatype
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

  def sort[T: ClassTag](vector: Vector[T], context: KeySortContext): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, context)
    vector
  }

  def sort[T: ClassTag](vector: Vector[T]): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length)
    vector
  }

  def sort[T: ClassTag](vector: Vector[T], order: SortOrder, context: KeySortContext): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, context)
    if (order == SortOrder.ASC) {
      vector
    } else {
      vector.reverse()
    }
  }

  def sort[T: ClassTag](vector: Vector[T], order: SortOrder): Unit = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length)
    if (order == SortOrder.ASC) {
      vector
    } else {
      vector.reverse()
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], context: KeyValueSortContext): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, context)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V]): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], order: SortOrder, context: KeyValueSortContext): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, context)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(), values.reverse())
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], order: SortOrder): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      (keys.reverse(), values.reverse())
    }
  }

  def sort[T: ClassTag](vector: Vector[T], segments: Vector[Int], context: KeySortContext): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, segments.deviceDataPtr, segments.length, context)
    vector
  }

  def sort[T: ClassTag](vector: Vector[T], segments: Vector[Int]): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, segments.deviceDataPtr, segments.length)
    vector
  }

  def sort[T: ClassTag](vector: Vector[T], segments: Vector[Int], order: SortOrder, context: KeySortContext): Vector[T] = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, segments.deviceDataPtr, segments.length, context)
    if (order == SortOrder.ASC) {
      vector
    } else {
      ???
    }
  }

  def sort[T: ClassTag](vector: Vector[T], segments: Vector[Int], order: SortOrder): Unit = {
    Sorting.sort(vector.deviceDataPtr, datatype(), vector.length, segments.deviceDataPtr, segments.length)
    if (order == SortOrder.ASC) {
      vector
    } else {
      ???
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], segments: Vector[Int], context: KeyValueSortContext): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, segments.deviceDataPtr, segments.length, context)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], segments: Vector[Int]): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, segments.deviceDataPtr, segments.length)
    (keys, values)
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], segments: Vector[Int], order: SortOrder, context: KeyValueSortContext): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, segments.deviceDataPtr, segments.length, context)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      ???
    }
  }

  def sort[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V], segments: Vector[Int], order: SortOrder): (Vector[K], Vector[V]) = {
    Sorting.sort(keys.deviceDataPtr, datatype[K](), values.deviceDataPtr, datatype[V](), keys.length, segments.deviceDataPtr, segments.length)
    if (order == SortOrder.ASC) {
      (keys, values)
    } else {
      ???
    }
  }

  def argsort[T: ClassTag](vector: Vector[T], context: KeyValueSortContext): Vector[Int] = {
    val indices = vector.indices()
    sort(vector, indices, context)
    indices
  }

  def argsort[T: ClassTag](vector: Vector[T]): Vector[Int] = {
    val indices = vector.indices()
    sort[T, Int](vector, indices)
    indices
  }

  def argsort[T: ClassTag](vector: Vector[T], order: SortOrder, context: KeyValueSortContext): Vector[Int] = {
    val indices = vector.indices()
    sort(vector, indices, order, context)
    indices
  }

  def argsort[T: ClassTag](vector: Vector[T], order: SortOrder): Vector[Int] = {
    val indices = vector.indices()
    sort(vector, indices, order)
    indices
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
