package ru.albemuth.tentura.tensor

import jcuda.Pointer
import jcuda.jcublas.{JCublas2, cublasHandle, cublasOperation}
import org.scalatest.FunSuite
import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.matrix.{MatrixKernel, MatrixMulMatrix}
import ru.albemuth.tentura.tensor.Scalar._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <vkornyshev@at-consulting.ru>}
  */
class TestMulMatrix extends FunSuite with TestUtils with TestWithResult {

  lazy val matrixMulMatrix = new KernelTemplate(new MatrixMulMatrix)

  def mmKernel[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MatrixKernel.matrix2_r(matrixMulMatrix, matrix1, matrix2, result)
  }

  def mmCublas(a: Matrix[Float], b: Matrix[Float], c: Matrix[Float])(implicit handle: cublasHandle): Matrix[Float] = {
    JCublas2.cublasSgemm(
      handle,
      cublasOperation.CUBLAS_OP_N, cublasOperation.CUBLAS_OP_N,
      b.columns, a.rows, a.columns,
      Pointer.to(Array[Float](1.0f)), b.deviceDataPtr, b.columns,
      a.deviceDataPtr, a.columns, Pointer.to(Array[Float](0.0f)),
      c.deviceDataPtr, c.columns
    )
    c
  }

  def mmCublasEx(a: Matrix[Float], b: Matrix[Float], c: Matrix[Float])(implicit handle: cublasHandle): Matrix[Float] = {
    JCublas2.cublasSgemm(
      handle,
      cublasOperation.CUBLAS_OP_T, cublasOperation.CUBLAS_OP_N,
      b.rows, a.rows, a.columns,
      Pointer.to(Array[Float](2.0f)), b.deviceDataPtr, b.columns,
      a.deviceDataPtr, a.columns, Pointer.to(Array[Float](0.0f)),
      c.deviceDataPtr, c.columns
    )
    c
  }

  test("mm kernel") {
    val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val data2 = NativeMatrix.matrixData[Float](COLUMNS, ROWS)
    val a = Matrix.of(data1)
    val b = Matrix.of(data2)
    val c = new Matrix[Float](ROWS, ROWS)
    val result = mmKernel(a, b, c)
  }

  test("mm cublas2") {
//    val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
//    val data1 = NativeMatrix.matrixData[Float](ROWS + 100, COLUMNS)
    val data1 = NativeMatrix.matrixData[Float](1000, 3072)
//    val data1 = NativeMatrix.matrixData[Float](2, 3)
//    val data1 = Array(Array(10000.0f, -20000.0f, -30000.0f), Array(40000.0f, 50000.0f, 60000.0f))
//    val data1 = Array(Array(1.0f, -2.0f, -3.0f), Array(1.0f, 1.0f, 1.0f))
//    val data1 = Array(Array(1.0f, -2.0f), Array(1.0f, 1.0f))
//    val data1 = Array(Array(1.0f, -1.0f), Array(1.0f, 1.0f))
//    val data1 = Array(Array(1.0f, -2.0f, -3.0f))
//    val data1 = Array(Array(236.59747f,  -68.20145f, -79.44149f), Array(1.0f, 2.0f, 3.0f))
//    val data2 = NativeMatrix.matrixData[Float](COLUMNS, ROWS - 100)
    val data2 = NativeMatrix.matrixData[Float](4000, 3072)
//    val data2 = NativeMatrix.matrixData[Float](3, 2)
//    val data2 = Array(Array(10000.0f, 20000.0f), Array(-30000.0f, 40000.0f), Array(-50000.0f, 60000.0f))
//    val data2 = Array(Array(1.0f, 1.0f), Array(-3.0f, 1.0f), Array(-5.0f, 1.0f))
//    val data2 = Array(Array(1.0f, 1.0f), Array(-2.0f, 1.0f))
//    val data2 = Array(Array(1.0f), Array(-3.0f), Array(-5.0f))
//    val data2 = Array(Array(148.42577f, 4.0f),  Array(-8.257609f, 5.0f), Array(-244.55411f, 6.0f))

    val a = Matrix.of(data1)
    val b = Matrix.of(data2)
//    val c = new Matrix[Float](a.rows, b.columns)
    val c = new Matrix[Float](a.rows, b.rows)
    implicit val handle = new cublasHandle
    JCublas2.cublasCreate(handle)
//    val resultCublas = mmCublas(a, b, c)
//    val resultCublas = mmCublasEx(a, b, c)
    val resultCublas = MatrixFunctions.gemm(2, a, cublasOperation.CUBLAS_OP_N, b, cublasOperation.CUBLAS_OP_T, 0, c)

    val m1 = Matrix.of(data1)
    val m2 = Matrix.of(data2)
//    val m3 = new Matrix[Float](a.rows, b.columns)
    val m3 = new Matrix[Float](a.rows, b.rows)
//    val resultKernel = mmKernel(m1, m2, m3)
    val resultKernel = 2 * m1 * m2.T

    val maxError = compare(resultCublas.values(), resultKernel.values())
    assert(maxError === 0)

    {
      val t1 = System.nanoTime()
//      val t1 = System.currentTimeMillis()
//      val rc = mmCublasEx(a, b, c)
      val rc = MatrixFunctions.gemm(2, a, cublasOperation.CUBLAS_OP_N, b, cublasOperation.CUBLAS_OP_T, 0, c)
      val t2 = System.nanoTime()
//      val t2 = System.currentTimeMillis()
      println(s"mm cublas2 ${t2 - t1}")
    }

    {
      val t1 = System.nanoTime()
//      val t1 = System.currentTimeMillis()
//      val rk = mmKernel(m1, m2, m3)
      val rk = 2 * m1 * m2.T
      val t2 = System.nanoTime()
//      val t2 = System.currentTimeMillis()
      println(s"mm kernel ${t2 - t1}")
    }
  }

}



