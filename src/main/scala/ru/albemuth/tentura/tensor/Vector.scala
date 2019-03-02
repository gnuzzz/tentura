package ru.albemuth.tentura.tensor

import Vector._
import jcuda.driver.CUdeviceptr
import ru.albemuth.jcuda.jcusegsort.{KeySortContext, KeyValueSortContext, Sorting}
import ru.albemuth.tentura.kernel.JCudaKernel.datatype
import ru.albemuth.tentura.DeviceVar
import ru.albemuth.tentura.kernel.JCudaKernel.{devicePtr, sizeOf}
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Math.{pow, pow2, pow2d, powd}
import ru.albemuth.tentura.tensor.Operator.Operator
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.kernel.matrix.MatrixKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel
import ru.albemuth.tentura.tensor.kernel.scalar.ScalarKernel.{scalar, scalar_r}
import ru.albemuth.tentura.tensor.kernel.vector._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class Vector[T: ClassTag](override val deviceDataPtr: CUdeviceptr, val length: Int) extends DeviceVar[T] {

  def this(l: Int) {
    this(devicePtr(l), l)
  }

  def this(data: Array[T]) {
    this(devicePtr(data.length), data.length)
    copy2device(data)
  }

  def apply(i: Int): Scalar[T] = {
    result("apply", i, new Scalar(deviceDataPtr.withByteOffset(i * sizeOf())))
  }

  def apply(from: Int, to: Int): Vector[T] = {
    result("apply", (from, to), new Vector(deviceDataPtr.withByteOffset(from * sizeOf()), to - from))
  }

  def update(i: Int, value: T): Unit = {
    copy2device(value, i)
  }

  def update(values: Vector[T]): Unit = {
    copyFrom(values, values.length)
  }

  def update(from: Int, to: Int, values: Vector[T]): Unit = {
    copyFrom(values, 0, from, to - from)
  }

  def slice(from: Int, to: Int): Vector[T] = {
    val r = this.result(Vector.slice, (from, to), new Vector[T](to - from))
    r.copyFrom(this, from, 0, to - from)
    r
  }

  def slice(from: Int, to: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(Vector.slice, this, from, to, result)
  }

  def concat(vector: Vector[T]): Vector[T] = {
    ??? //todo
  }

  def values(): Array[T] = {
    val data = Array.ofDim[T](length)
    copy2host(data)
    data
  }

  def values(indices: Vector[Int]): Vector[T] = {
    VectorKernel.vector2(Vector.values, this, indices, new Vector[T](indices.length))
  }

  def values(indices: Vector[Int], result: Vector[T]): Vector[T] = {
    VectorKernel.vector2_r(Vector.values, this, indices, result)
  }

  def values(indices: Matrix[Int]): Matrix[T] = {
    MatrixKernel.matrix2(vectorValuesMatrix, this, indices, new Matrix[T](indices.rows, indices.columns))
  }

  def values(indices: Matrix[Int], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2_r(vectorValuesMatrix, this, indices, result)
  }

  def +(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowAddMatrix, this, matrix, result)
  }

  def +|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnAddMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def +|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnAddMatrix, this, matrix, result)
  }

  def +(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorAddVector, this, vector, new Vector[T](length))
  }

  def +(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddVector, this, vector, result)
  }

  def +=(vector: Vector[T]): Unit = {
    VectorKernel.vector_r(vectorAddVector, this, vector, this)
  }

  def +(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Byte): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def +(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Short): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def +(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Int): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def +(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Long): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def +(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Float): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def +(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorAddScalar, this, scalar, new Vector[T](length))
  }

  def +(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, result)
  }

  def +=(scalar: Double): Unit = {
    VectorKernel.vector_r(vectorAddScalar, this, scalar, this)
  }

  def -(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowSubMatrix, this, matrix, result)
  }

  def -|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnSubMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def -|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnSubMatrix, this, matrix, result)
  }

  def -(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorSubVector, this, vector, new Vector[T](length))
  }

  def -(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubVector, this, vector, result)
  }

  def -=(vector: Vector[T]): Unit = {
    VectorKernel.vector_r(vectorSubVector, this, vector, this)
  }

  def -(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Byte): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def -(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Short): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def -(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Int): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def -(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Long): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def -(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Float): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def -(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorSubScalar, this, scalar, new Vector[T](length))
  }

  def -(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, result)
  }

  def -=(scalar: Double): Unit = {
    VectorKernel.vector_r(vectorSubScalar, this, scalar, this)
  }

  def *(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Byte): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def *(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Short): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def *(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Int): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def *(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Long): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def *(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Float): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def *(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorTimesScalar, this, scalar, new Vector[T](length))
  }

  def *(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, result)
  }

  def *=(scalar: Double): Unit = {
    VectorKernel.vector_r(vectorTimesScalar, this, scalar, this)
  }

  def ***(vector: Vector[T]): Scalar[T] = {
    ScalarKernel.scalar(vectorDotVector, this, vector, new Scalar[T]())
  }

  def ***(vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    ScalarKernel.scalar_r(vectorDotVector, this, vector, result)
  }

  def ***(matrix: Matrix[T]): Vector[T] = {
    VectorKernel.vector(vectorDotMatrix, this, matrix, new Vector[T](matrix.columns))
  }

  def ***(matrix: Matrix[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDotMatrix, this, matrix, result)
  }

  def *(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowTimesMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def *(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowTimesMatrix, this, matrix, result)
  }

  def *|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnTimesMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def *|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnTimesMatrix, this, matrix, result)
  }

  def ***|(vector: Vector[T]): Matrix[T] = {
    MatrixKernel.matrix(columnDotRow, this, vector, new Matrix[T](length, vector.length))
  }

  def ***|(vector: Vector[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnDotRow, this, vector, result)
  }

  def *(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorTimesVector, this, vector, new Vector[T](length))
  }

  def *(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorTimesVector, this, vector, result)
  }

  def *=(vector: Vector[T]): Unit = {
    VectorKernel.vector_r(vectorTimesVector, this, vector, this)
  }

  def /(scalar: Byte): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Byte, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Byte): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(scalar: Short): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Short, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Short): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(scalar: Int): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Int, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Int): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(scalar: Long): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Long, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Long): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(scalar: Float): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Float, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Float): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(scalar: Double): Vector[T] = {
    VectorKernel.vector(vectorDivScalar, this, scalar, new Vector[T](length))
  }

  def /(scalar: Double, result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, result)
  }

  def /=(scalar: Double): Unit = {
    VectorKernel.vector_r(vectorDivScalar, this, scalar, this)
  }

  def /(vector: Vector[T]): Vector[T] = {
    VectorKernel.vector(vectorDivVector, this, vector, new Vector[T](length))
  }

  def /(vector: Vector[T], result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(vectorDivVector, this, vector, result)
  }

  def /=(vector: Vector[T]): Unit = {
    VectorKernel.vector_r(vectorDivVector, this, vector, this)
  }

  def /(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(rowDivMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def /(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(rowDivMatrix, this, matrix, result)
  }

  def /|(matrix: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix(columnDivMatrix, this, matrix, new Matrix[T](matrix.rows, matrix.columns))
  }

  def /|(matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix_r(columnDivMatrix, this, matrix, result)
  }

  def *^(power: Float): Vector[Float] = {
    if (power == 2) {
      pow2(this)
    } else {
      pow(this, power)
    }
  }

  def *^(power: Double): Vector[Double] = {
    if (power == 2) {
      pow2d(this)
    } else {
      powd(this, power)
    }
  }

  def sum(): Scalar[T] = {
    Vector.sum(this)
  }

  def mean(): Scalar[Float] = {
    Vector.mean(this)
  }

  def meand(): Scalar[Double] = {
    Vector.meand(this)
  }

  def std(): Scalar[Float] = {
    Vector.std(this)
  }

  def stdd(): Scalar[Double] = {
    Vector.stdd(this)
  }

  def max(): Scalar[T] = {
    Vector.max(this)
  }

  def min(): Scalar[T] = {
    Vector.min(this)
  }

  def argmax(): Scalar[Int] = {
    Vector.argmax(this)
  }

  def argmin(): Scalar[Int] = {
    Vector.argmin(this)
  }

  def indices(): Vector[Int] = {
    VectorKernel.vector(Vector.indices, this, new Vector[Int](length))
  }

  def reverse(): Vector[T] = {
    VectorKernel.vector(Vector.reverse, this, new Vector[T](length))
  }

  def reverse(result: Vector[T]): Vector[T] = {
    VectorKernel.vector_r(Vector.reverse, this, result)
  }

}

object Vector {

  //kernels for vector instances
  private lazy val vectorAddVector = new KernelTemplate(new VectorAddVector)
  private lazy val vectorAddScalar = new KernelTemplate(new VectorAddScalar)
  private lazy val vectorSubVector = new KernelTemplate(new VectorSubVector)
  private lazy val vectorSubScalar = new KernelTemplate(new VectorSubScalar)
  private lazy val vectorTimesScalar = new KernelTemplate(new VectorTimesScalar)
  private lazy val vectorDotVector = new KernelTemplate(new VectorDotVector)
  private lazy val vectorDotMatrix = new KernelTemplate(new VectorDotMatrix)
  private lazy val columnDotRow = new KernelTemplate(new ColumnDotRow)
  private lazy val vectorDivVector = new KernelTemplate(new VectorDivVector)
  private lazy val vectorTimesVector = new KernelTemplate(new VectorTimesVector)
  private lazy val vectorDivScalar = new KernelTemplate(new VectorDivScalar)
  private lazy val sum = new KernelTemplate(new Sum)
  private lazy val rowAddMatrix = new KernelTemplate(new RowAddMatrix)
  private lazy val columnAddMatrix = new KernelTemplate(new ColumnAddMatrix)
  private lazy val rowSubMatrix = new KernelTemplate(new RowSubMatrix)
  private lazy val columnSubMatrix = new KernelTemplate(new ColumnSubMatrix)
  private lazy val rowTimesMatrix = new KernelTemplate(new RowTimesMatrix)
  private lazy val columnTimesMatrix = new KernelTemplate(new ColumnTimesMatrix)
  private lazy val rowDivMatrix = new KernelTemplate(new RowDivMatrix)
  private lazy val columnDivMatrix = new KernelTemplate(new ColumnDivMatrix)
  private lazy val value = new KernelTemplate(new Value)
  private lazy val slice = new KernelTemplate(new Slice)
  private lazy val values = new KernelTemplate(new Values)
  private lazy val indices = new KernelTemplate(new Indices)
  private lazy val vectorValuesMatrix = new KernelTemplate(new VectorValuesMatrix)
  private lazy val reverse = new KernelTemplate(new Reverse())

  //kernels for vector functions
  private lazy val bincount = new Bincount
  private lazy val mean = new KernelTemplate(new Mean("mean"))
  private lazy val meand = new KernelTemplate(new Mean("meand"))
  private lazy val std = new KernelTemplate(new Std("math_std"))
  private lazy val stdd = new KernelTemplate(new Stdd("math_stdd"))
  private lazy val max = new KernelTemplate(new Max)
  private lazy val min = new KernelTemplate(new Min)
  private lazy val argmax = new KernelTemplate(new Argmax)
  private lazy val argmin = new KernelTemplate(new Argmin)

  //kernels for vector cas functions with one operand
  private lazy val casScalarThresholdScalarOperand = new KernelTemplate(new Cas("Cas", "cas_scalar_threshold_scalar_operand"))
  private lazy val casScalarThresholdVectorOperand = new KernelTemplate(new Cas("Cas", "cas_scalar_threshold_vector_operand"))
  private lazy val casVectorThresholdScalarOperand = new KernelTemplate(new Cas("Cas", "cas_vector_threshold_scalar_operand"))
  private lazy val casVectorThresholdVectorOperand = new KernelTemplate(new Cas("Cas", "cas_vector_threshold_vector_operand"))

  //kernels for vector cas functions with two operands
  private lazy val casScalarThresholdScalarOperand1ScalarOperand2 = new KernelTemplate(new Cas("Cas2", "cas_scalar_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casScalarThresholdVectorOperand1VectorOperand2 = new KernelTemplate(new Cas("Cas2", "cas_scalar_threshold_vector_operand1_vector_operand_2"))
  private lazy val casVectorThresholdScalarOperand1ScalarOperand2 = new KernelTemplate(new Cas("Cas2", "cas_vector_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casVectorThresholdVectorOperand1VectorOperand2 = new KernelTemplate(new Cas("Cas2", "cas_vector_threshold_vector_operand1_vector_operand2"))

  //kernels for vector cas functions with one operand and indexed arguments
  private lazy val casScalarThresholdIndexedOperand = new KernelTemplate(new CasIndexed("Cas_indexed", "cas_scalar_threshold_indexed_operand"))
  private lazy val casIndexedThresholdScalarOperand = new KernelTemplate(new CasIndexed("Cas_indexed", "cas_indexed_threshold_scalar_operand"))
  private lazy val casIndexedThresholdIndexedOperand = new KernelTemplate(new CasIndexed("Cas_indexed", "cas_indexed_threshold_indexed_operand"))

  //kernels for vector cas functions with two operands and indexed arguments
  private lazy val casScalarThresholdIndexedOperand1IndexedOperand2 = new KernelTemplate(new CasIndexed("Cas_indexed2", "cas_scalar_threshold_indexed_operand1_indexed_operand2"))
  private lazy val casIndexedThresholdScalarOperand1ScalarOperand2 = new KernelTemplate(new CasIndexed("Cas_indexed2", "cas_indexed_threshold_scalar_operand1_scalar_operand2"))
  private lazy val casIndexedThresholdIndexedOperand1IndexedOperand2 = new KernelTemplate(new CasIndexed("Cas_indexed2", "cas_indexed_threshold_indexed_operand1_indexed_operand2"))

  def apply[T: ClassTag](length: Int): VectorBuilder[T] = new VectorBuilder[T](length)

  def of[T: ClassTag](values: Array[T]): Vector[T] = {
    new Vector(values)
  }

  def bincount(vector: Vector[Int]): Vector[Int] = {
    val maxValue = max(vector).value()
    bincount(vector, maxValue)
  }

  def bincount(vector: Vector[Int], maxValue: Int, result: Vector[Int]): Vector[Int] = {
    VectorKernel.vector_r(bincount, vector, maxValue, result)
  }

  def bincount(vector: Vector[Int], maxValue: Int): Vector[Int] = {
    val result = vector.result(bincount, maxValue, new Vector[Int](maxValue + 1))
    VectorKernel.vector_r(bincount, vector, maxValue, result)
  }

  def keySortContext[T: ClassTag](vector: Vector[T]): KeySortContext = {
    Sorting.keySortContext(datatype(), vector.length, 1)
  }

  def keyValueSortContext[K: ClassTag, V: ClassTag](keys: Vector[K], values: Vector[V]): KeyValueSortContext = {
    Sorting.keyValueSortContext(datatype[K](), datatype[V](), keys.length, 1)
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

  def sort[T: ClassTag](vector: Vector[T], order: SortOrder): Vector[T] = {
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
    scalar_r(sum, vector, result)
  }

  def sum[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(sum, vector)
  }

  def mean[T: ClassTag](vector: Vector[T], result: Scalar[Float]): Scalar[Float] = {
    scalar_r(mean, vector, result)
  }

  def mean[T: ClassTag](vector: Vector[T]): Scalar[Float] = {
    scalar(mean, vector)
  }

  def meand[T: ClassTag](vector: Vector[T], result: Scalar[Double]): Scalar[Double] = {
    scalar_r(meand, vector, result)
  }

  def meand[T: ClassTag](vector: Vector[T]): Scalar[Double] = {
    scalar(meand, vector)
  }

  def std[T: ClassTag](vector: Vector[T], result: Scalar[Float]): Scalar[Float] = {
    scalar_r(std, vector, result)
  }

  def std[T: ClassTag](vector: Vector[T]): Scalar[Float] = {
    scalar(std, vector)
  }

  def stdd[T: ClassTag](vector: Vector[T], result: Scalar[Double]): Scalar[Double] = {
    scalar_r(stdd, vector, result)
  }

  def stdd[T: ClassTag](vector: Vector[T]): Scalar[Double] = {
    scalar(stdd, vector)
  }

  def max[T: ClassTag](vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(max, vector, result)
  }

  def max[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(max, vector)
  }

  def min[T: ClassTag](vector: Vector[T], result: Scalar[T]): Scalar[T] = {
    scalar_r(min, vector, result)
  }

  def min[T: ClassTag](vector: Vector[T]): Scalar[T] = {
    scalar(min, vector)
  }

  def argmax[T: ClassTag](vector: Vector[T], result: Scalar[Int]): Scalar[Int] = {
    scalar_r(argmax, vector, result)
  }

  def argmax[T: ClassTag](vector: Vector[T]): Scalar[Int] = {
    scalar(argmax, vector)
  }

  def argmin[T: ClassTag](vector: Vector[T], result: Scalar[Int]): Scalar[Int] = {
    scalar_r(argmin, vector, result)
  }

  def argmin[T: ClassTag](vector: Vector[T]): Scalar[Int] = {
    scalar(argmin, vector)
  }

  class VectorBuilder[T: ClassTag](length: Int) {

    def of(value: => T): Vector[T] = {
      new Vector(Array.fill(length)(value))
    }

  }

  object Cas {

    object Indexed {

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casScalarThresholdIndexedOperand, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casScalarThresholdIndexedOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: T, indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdScalarOperand, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: T, indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdScalarOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdIndexedOperand, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdIndexedOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casScalarThresholdIndexedOperand, vector, comparator, threshold, operator, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casScalarThresholdIndexedOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdScalarOperand, vector, comparator, threshold, operator, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdScalarOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdIndexedOperand, vector, comparator, threshold, operator, operand, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdIndexedOperand.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator, operand), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator, operand, indices, result)
      }

    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdScalarOperand, vector, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: T): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdScalarOperand, vector, comparator, threshold, Operator.ASSIGN, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdVectorOperand, vector, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdVectorOperand, vector, comparator, threshold, Operator.ASSIGN, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdScalarOperand, vector, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: T): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdScalarOperand, vector, comparator, threshold, Operator.ASSIGN, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdVectorOperand, vector, comparator, threshold, Operator.ASSIGN, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdVectorOperand, vector, comparator, threshold, Operator.ASSIGN, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdScalarOperand, vector, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: T): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdScalarOperand, vector, comparator, threshold, operator, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdVectorOperand, vector, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator: Operator, operand: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdVectorOperand, vector, comparator, threshold, operator, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdScalarOperand, vector, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: T): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdScalarOperand, vector, comparator, threshold, operator, operand, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdVectorOperand, vector, comparator, threshold, operator, operand, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator: Operator, operand: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdVectorOperand, vector, comparator, threshold, operator, operand, new Vector[T](vector.length))
    }

  }

  object Cas2 {

    object Indexed {

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casScalarThresholdIndexedOperand1IndexedOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casScalarThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdScalarOperand1ScalarOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdIndexedOperand1IndexedOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casScalarThresholdIndexedOperand1IndexedOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casScalarThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdScalarOperand1ScalarOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int], result: Vector[T]): Vector[T] = {
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(casIndexedThresholdIndexedOperand1IndexedOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

      def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], indices: Vector[Int]): Vector[T] = {
        val kernel = casIndexedThresholdIndexedOperand1IndexedOperand2.kernel[T]
        val result = vector.result(kernel, (comparator, threshold, operator1, operand1, operator2, operand2), new Vector[T](vector.length))
        result.copyFrom(vector, vector.length)
        CasIndexed.vector_r(kernel, vector, comparator, threshold, operator1, operand1, operator2, operand2, indices, result)
      }

    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: T, operand2: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: T, operand2: T): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operand1: Vector[T], operand2: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: T, operand2: T): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operand1: Vector[T], operand2: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, Operator.ASSIGN, operand1, Operator.ASSIGN, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: T, operator2: Operator, operand2: T): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casScalarThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: T, operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casScalarThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T, result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: T, operator2: Operator, operand2: T): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdScalarOperand1ScalarOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, new Vector[T](vector.length))
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T], result: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector_r(casVectorThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, result)
    }

    def cas[T: ClassTag](vector: Vector[T], comparator: Comparator, threshold: Vector[T], operator1: Operator, operand1: Vector[T], operator2: Operator, operand2: Vector[T]): Vector[T] = {
      kernel.vector.Cas.vector(casVectorThresholdVectorOperand1VectorOperand2, vector, comparator, threshold, operator1, operand1, operator2, operand2, new Vector[T](vector.length))
    }

  }

}