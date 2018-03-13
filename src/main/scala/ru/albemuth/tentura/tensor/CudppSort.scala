package ru.albemuth.tentura.tensor

import jcuda.jcudpp._
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.CudppOperation.{cudppConfig, cudppPlan, datatype}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object CudppSort {

  def sort[T: ClassTag](vector: Vector[T]): SortOperation = {
    radixSort(vector)
  }

  def sort[T: ClassTag](vector: Vector[T], order: SortOrder): SortOperation = {
    radixSort(vector, order)
  }

  def argsort[T: ClassTag](vector: Vector[T]): ArgsortOperation = {
    radixArgsort(vector)
  }

  def argsort[T: ClassTag](vector: Vector[T], order: SortOrder): ArgsortOperation = {
    radixArgsort(vector, order)
  }

  def mergeSort[T: ClassTag](vector: Vector[T]): MergeSort = {
    mergeSort(vector, SortOrder.ASC)
  }

  def mergeSort[T: ClassTag](vector: Vector[T], order: SortOrder): MergeSort = {
    val orderOption = if (order == SortOrder.ASC) CUDPPOption.CUDPP_OPTION_FORWARD else CUDPPOption.CUDPP_OPTION_BACKWARD
    val config = cudppConfig(CUDPPAlgorithm.CUDPP_SORT_MERGE, datatype(vector), CUDPPOperator.CUDPP_MIN, CUDPPOption.CUDPP_OPTION_KEYS_ONLY | orderOption)
    val plan = cudppPlan(config, vector.length, 1, 0)
    new MergeSort(plan, vector.length)
  }

  def mergeArgsort[T: ClassTag](vector: Vector[T]): MergeSort = {
    mergeArgsort(vector, SortOrder.ASC)
  }

  def mergeArgsort[T: ClassTag](vector: Vector[T], order: SortOrder): MergeSort = {
    val orderOption = if (order == SortOrder.ASC) CUDPPOption.CUDPP_OPTION_FORWARD else CUDPPOption.CUDPP_OPTION_BACKWARD
    val config = cudppConfig(CUDPPAlgorithm.CUDPP_SORT_MERGE, datatype(vector), CUDPPOperator.CUDPP_MIN, CUDPPOption.CUDPP_OPTION_KEY_VALUE_PAIRS | orderOption)
    val plan = cudppPlan(config, vector.length, 1, 0)
    new MergeSort(plan, vector.length)
  }

  def radixSort[T: ClassTag](vector: Vector[T]): RadixSort = {
    radixSort(vector, SortOrder.ASC)
  }

  def radixSort[T: ClassTag](vector: Vector[T], order: SortOrder): RadixSort = {
    val orderOption = if (order == SortOrder.ASC) CUDPPOption.CUDPP_OPTION_FORWARD else CUDPPOption.CUDPP_OPTION_BACKWARD
    val config = cudppConfig(CUDPPAlgorithm.CUDPP_SORT_RADIX, datatype(vector), CUDPPOperator.CUDPP_MIN, CUDPPOption.CUDPP_OPTION_KEYS_ONLY | orderOption)
    val plan = cudppPlan(config, vector.length, 1, 0)
    new RadixSort(plan, vector.length)
  }

  def radixArgsort[T: ClassTag](vector: Vector[T]): RadixArgsort = {
    radixArgsort(vector, SortOrder.ASC)
  }

  def radixArgsort[T: ClassTag](vector: Vector[T], order: SortOrder): RadixArgsort = {
    val orderOption = if (order == SortOrder.ASC) CUDPPOption.CUDPP_OPTION_FORWARD else CUDPPOption.CUDPP_OPTION_BACKWARD
    val config = cudppConfig(CUDPPAlgorithm.CUDPP_SORT_RADIX, datatype(vector), CUDPPOperator.CUDPP_MIN, CUDPPOption.CUDPP_OPTION_KEY_VALUE_PAIRS | orderOption)
    val plan = cudppPlan(config, vector.length, 1, 0)
    new RadixArgsort(plan, vector.length)
  }

  class MergeSort(override val plan: CUDPPHandle, val maxLength: Int) extends CudppOperation(plan) with SortOperation {

    override def apply[T](vector: Vector[T]): Vector[T] = {
      if (vector.length > maxLength) throw new IllegalArgumentException(s"vector length ${vector.length} is more than maximum length $maxLength for merge sort operation")
      JCudpp.cudppMergeSort(plan, vector.deviceDataPtr, null, vector.length)
      vector
    }
  }

  class MergeArgsort(override val plan: CUDPPHandle, val maxLength: Int) extends CudppOperation(plan) with ArgsortOperation {

    override def apply[T, R](vector: Vector[T], args: Vector[R]): Vector[R] = {
      if (vector.length > maxLength) throw new IllegalArgumentException(s"vector length ${vector.length} is more than maximum length $maxLength for merge argsort operation")
      JCudpp.cudppMergeSort(plan, vector.deviceDataPtr, args.deviceDataPtr, vector.length)
      args
    }

  }

  class RadixSort(override val plan: CUDPPHandle, val maxLength: Int) extends CudppOperation(plan) with SortOperation {

    override def apply[T](vector: Vector[T]): Vector[T] = {
      if (vector.length > maxLength) throw new IllegalArgumentException(s"vector length ${vector.length} is more than maximum length $maxLength for radix sort operation")
      JCudpp.cudppRadixSort(plan, vector.deviceDataPtr, null, vector.length)
      vector
    }

  }

  class RadixArgsort(override val plan: CUDPPHandle, val maxLength: Int) extends CudppOperation(plan) with ArgsortOperation {

    override def apply[T, R](vector: Vector[T], args: Vector[R]): Vector[R] = {
      if (vector.length > maxLength) throw new IllegalArgumentException(s"vector length ${vector.length} is more than maximum length $maxLength for radix argsort operation")
      JCudpp.cudppRadixSort(plan, vector.deviceDataPtr, args.deviceDataPtr, vector.length)
      args
    }

  }

}
