package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.math.MathKernel

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object Math {

  def abs[T: ClassTag](vector: Vector[T], result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(abs, vector, result)
  }

  def abs[T: ClassTag](vector: Vector[T]): Vector[T] = {
    MathKernel.vector(abs, vector)
  }

  def abs[T: ClassTag](matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(abs, matrix, result)
  }

  def abs[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    MathKernel.matrix(abs, matrix)
  }

  def acos[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(acosf, vector, result)
  }

  def acos[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(acosf, vector)
  }

  def acos(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    acosd(vector, result)
  }

  def acos(vector: Vector[Double]): Vector[Double] = {
    acosd(vector)
  }

  def acosd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(acosd, vector, result)
  }

  def acosd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(acosd, vector)
  }

  def acos[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(acosf, matrix, result)
  }

  def acos[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(acosf, matrix)
  }

  def acos(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    acosd(matrix, result)
  }

  def acos(matrix: Matrix[Double]): Matrix[Double] = {
    acosd(matrix)
  }

  def acosd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(acosd, matrix, result)
  }

  def acosd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(acosd, matrix)
  }

  def acosh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(acoshf, vector, result)
  }

  def acosh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(acoshf, vector)
  }

  def acosh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    acoshd(vector, result)
  }

  def acosh(vector: Vector[Double]): Vector[Double] = {
    acoshd(vector)
  }

  def acoshd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(acoshd, vector, result)
  }

  def acoshd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(acoshd, vector)
  }

  def acosh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(acoshf, matrix, result)
  }

  def acosh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(acoshf, matrix)
  }

  def acosh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    acoshd(matrix, result)
  }

  def acosh(matrix: Matrix[Double]): Matrix[Double] = {
    acoshd(matrix)
  }

  def acoshd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(acoshd, matrix, result)
  }

  def acoshd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(acoshd, matrix)
  }

  def asin[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(asinf, vector, result)
  }

  def asin[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(asinf, vector)
  }

  def asin(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    asind(vector, result)
  }

  def asin(vector: Vector[Double]): Vector[Double] = {
    asind(vector)
  }

  def asind[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(asind, vector, result)
  }

  def asind[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(asind, vector)
  }

  def asin[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(asinf, matrix, result)
  }

  def asin[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(asinf, matrix)
  }

  def asin(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    asind(matrix, result)
  }

  def asin(matrix: Matrix[Double]): Matrix[Double] = {
    asind(matrix)
  }

  def asind[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(asind, matrix, result)
  }

  def asind[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(asind, matrix)
  }

  def asinh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(asinhf, vector, result)
  }

  def asinh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(asinhf, vector)
  }

  def asinh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    asinhd(vector, result)
  }

  def asinh(vector: Vector[Double]): Vector[Double] = {
    asinhd(vector)
  }

  def asinhd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(asinhd, vector, result)
  }

  def asinhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(asinhd, vector)
  }

  def asinh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(asinhf, matrix, result)
  }

  def asinh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(asinhf, matrix)
  }

  def asinh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    asinhd(matrix, result)
  }

  def asinh(matrix: Matrix[Double]): Matrix[Double] = {
    asinhd(matrix)
  }

  def asinhd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(asinhd, matrix, result)
  }

  def asinhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(asinhd, matrix)
  }

  def atan[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(atanf, vector, result)
  }

  def atan[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(atanf, vector)
  }

  def atan(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    atand(vector, result)
  }

  def atan(vector: Vector[Double]): Vector[Double] = {
    atand(vector)
  }

  def atand[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(atand, vector, result)
  }

  def atand[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(atand, vector)
  }

  def atan[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(atanf, matrix, result)
  }

  def atan[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(atanf, matrix)
  }

  def atan(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    atand(matrix, result)
  }

  def atan(matrix: Matrix[Double]): Matrix[Double] = {
    atand(matrix)
  }

  def atand[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(atand, matrix, result)
  }

  def atand[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(atand, matrix)
  }

  def atanh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(atanhf, vector, result)
  }

  def atanh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(atanhf, vector)
  }

  def atanh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    atanhd(vector, result)
  }

  def atanh(vector: Vector[Double]): Vector[Double] = {
    atanhd(vector)
  }

  def atanhd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(atanhd, vector, result)
  }

  def atanhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(atanhd, vector)
  }

  def atanh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(atanhf, matrix, result)
  }

  def atanh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(atanhf, matrix)
  }

  def atanh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    atanhd(matrix, result)
  }

  def atanh(matrix: Matrix[Double]): Matrix[Double] = {
    atanhd(matrix)
  }

  def atanhd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(atanhd, matrix, result)
  }

  def atanhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(atanhd, matrix)
  }

  def cbrt[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(cbrtf, vector, result)
  }

  def cbrt[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(cbrtf, vector)
  }

  def cbrt(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    cbrtd(vector, result)
  }

  def cbrt(vector: Vector[Double]): Vector[Double] = {
    cbrtd(vector)
  }

  def cbrtd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(cbrtd, vector, result)
  }

  def cbrtd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(cbrtd, vector)
  }

  def cbrt[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(cbrtf, matrix, result)
  }

  def cbrt[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(cbrtf, matrix)
  }

  def cbrt(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    cbrtd(matrix, result)
  }

  def cbrt(matrix: Matrix[Double]): Matrix[Double] = {
    cbrtd(matrix)
  }

  def cbrtd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(cbrtd, matrix, result)
  }

  def cbrtd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(cbrtd, matrix)
  }

  def ceil[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(ceilf, vector, result)
  }

  def ceil[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(ceilf, vector)
  }

  def ceil(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    ceild(vector, result)
  }

  def ceil(vector: Vector[Double]): Vector[Double] = {
    ceild(vector)
  }

  def ceild[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(ceild, vector, result)
  }

  def ceild[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(ceild, vector)
  }

  def ceil[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(ceilf, matrix, result)
  }

  def ceil[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(ceilf, matrix)
  }

  def ceil(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    ceild(matrix, result)
  }

  def ceil(matrix: Matrix[Double]): Matrix[Double] = {
    ceild(matrix)
  }

  def ceild[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(ceild, matrix, result)
  }

  def ceild[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(ceild, matrix)
  }

  def cos[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(cosf, vector, result)
  }

  def cos[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(cosf, vector)
  }

  def cos(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    cosd(vector, result)
  }

  def cos(vector: Vector[Double]): Vector[Double] = {
    cosd(vector)
  }

  def cosd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(cosd, vector, result)
  }

  def cosd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(cosd, vector)
  }

  def cos[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(cosf, matrix, result)
  }

  def cos[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(cosf, matrix)
  }

  def cos(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    cosd(matrix, result)
  }

  def cos(matrix: Matrix[Double]): Matrix[Double] = {
    cosd(matrix)
  }

  def cosd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(cosd, matrix, result)
  }

  def cosd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(cosd, matrix)
  }

  def cosh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(coshf, vector, result)
  }

  def cosh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(coshf, vector)
  }

  def cosh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    coshd(vector, result)
  }

  def cosh(vector: Vector[Double]): Vector[Double] = {
    coshd(vector)
  }

  def coshd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(coshd, vector, result)
  }

  def coshd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(coshd, vector)
  }

  def cosh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(coshf, matrix, result)
  }

  def cosh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(coshf, matrix)
  }

  def cosh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    coshd(matrix, result)
  }

  def cosh(matrix: Matrix[Double]): Matrix[Double] = {
    coshd(matrix)
  }

  def coshd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(coshd, matrix, result)
  }

  def coshd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(coshd, matrix)
  }

  def exp[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(expf, vector, result)
  }

  def exp[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(expf, vector)
  }

  def exp(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    expd(vector, result)
  }

  def exp(vector: Vector[Double]): Vector[Double] = {
    expd(vector)
  }

  def expd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(expd, vector, result)
  }

  def expd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(expd, vector)
  }

  def exp[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(expf, matrix, result)
  }

  def exp[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(expf, matrix)
  }

  def exp(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    expd(matrix, result)
  }

  def exp(matrix: Matrix[Double]): Matrix[Double] = {
    expd(matrix)
  }

  def expd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(expd, matrix, result)
  }

  def expd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(expd, matrix)
  }

  def exp10[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(exp10f, vector, result)
  }

  def exp10[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(exp10f, vector)
  }

  def exp10(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    exp10d(vector, result)
  }

  def exp10(vector: Vector[Double]): Vector[Double] = {
    exp10d(vector)
  }

  def exp10d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(exp10d, vector, result)
  }

  def exp10d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(exp10d, vector)
  }

  def exp10[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(exp10f, matrix, result)
  }

  def exp10[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(exp10f, matrix)
  }

  def exp10(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    exp10d(matrix, result)
  }

  def exp10(matrix: Matrix[Double]): Matrix[Double] = {
    exp10d(matrix)
  }

  def exp10d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(exp10d, matrix, result)
  }

  def exp10d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(exp10d, matrix)
  }

  def exp2[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(exp2f, vector, result)
  }

  def exp2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(exp2f, vector)
  }

  def exp2(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    exp2d(vector, result)
  }

  def exp2(vector: Vector[Double]): Vector[Double] = {
    exp2d(vector)
  }

  def exp2d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(exp2d, vector, result)
  }

  def exp2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(exp2d, vector)
  }

  def exp2[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(exp2f, matrix, result)
  }

  def exp2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(exp2f, matrix)
  }

  def exp2(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    exp2d(matrix, result)
  }

  def exp2(matrix: Matrix[Double]): Matrix[Double] = {
    exp2d(matrix)
  }

  def exp2d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(exp2d, matrix, result)
  }

  def exp2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(exp2d, matrix)
  }

  def floor[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(floorf, vector, result)
  }

  def floor[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(floorf, vector)
  }

  def floor(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    floord(vector, result)
  }

  def floor(vector: Vector[Double]): Vector[Double] = {
    floord(vector)
  }

  def floord[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(floord, vector, result)
  }

  def floord[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(floord, vector)
  }

  def floor[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(floorf, matrix, result)
  }

  def floor[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(floorf, matrix)
  }

  def floor(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    floord(matrix, result)
  }

  def floor(matrix: Matrix[Double]): Matrix[Double] = {
    floord(matrix)
  }

  def floord[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(floord, matrix, result)
  }

  def floord[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(floord, matrix)
  }

  def ln[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(lnf, vector, result)
  }

  def ln[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(lnf, vector)
  }

  def ln(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    lnd(vector, result)
  }

  def ln(vector: Vector[Double]): Vector[Double] = {
    lnd(vector)
  }

  def lnd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(lnd, vector, result)
  }

  def lnd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(lnd, vector)
  }

  def ln[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(lnf, matrix, result)
  }

  def ln[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(lnf, matrix)
  }

  def ln(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    lnd(matrix, result)
  }

  def ln(matrix: Matrix[Double]): Matrix[Double] = {
    lnd(matrix)
  }

  def lnd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(lnd, matrix, result)
  }

  def lnd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(lnd, matrix)
  }

  def log[T: ClassTag](vector: Vector[T], base: Float, result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(logf, vector, base, result)
  }

  def log[T: ClassTag](vector: Vector[T], base: Float): Vector[Float] = {
    MathKernel.vector(logf, vector, base)
  }

  def log(vector: Vector[Double], base: Double, result: Vector[Double]): Vector[Double] = {
    logd(vector, base, result)
  }

  def log(vector: Vector[Double], base: Double): Vector[Double] = {
    logd(vector, base)
  }

  def logd[T: ClassTag](vector: Vector[T], base: Double, result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(logd, vector, base, result)
  }

  def logd[T: ClassTag](vector: Vector[T], base: Double): Vector[Double] = {
    MathKernel.vector(logd, vector, base)
  }

  def log[T: ClassTag](matrix: Matrix[T], base: Float, result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(logf, matrix, base, result)
  }

  def log[T: ClassTag](matrix: Matrix[T], base: Float): Matrix[Float] = {
    MathKernel.matrix(logf, matrix, base)
  }

  def log(matrix: Matrix[Double], base: Double, result: Matrix[Double]): Matrix[Double] = {
    logd(matrix, base, result)
  }

  def log(matrix: Matrix[Double], base: Double): Matrix[Double] = {
    logd(matrix, base)
  }

  def logd[T: ClassTag](matrix: Matrix[T], base: Double, result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(logd, matrix, base, result)
  }

  def logd[T: ClassTag](matrix: Matrix[T], base: Double): Matrix[Double] = {
    MathKernel.matrix(logd, matrix, base)
  }

  def log10[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(log10f, vector, result)
  }

  def log10[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(log10f, vector)
  }

  def log10(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    log10d(vector, result)
  }

  def log10(vector: Vector[Double]): Vector[Double] = {
    log10d(vector)
  }

  def log10d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(log10d, vector, result)
  }

  def log10d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(log10d, vector)
  }

  def log10[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(log10f, matrix, result)
  }

  def log10[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(log10f, matrix)
  }

  def log10(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    log10d(matrix, result)
  }

  def log10(matrix: Matrix[Double]): Matrix[Double] = {
    log10d(matrix)
  }

  def log10d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(log10d, matrix, result)
  }

  def log10d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(log10d, matrix)
  }

  def log2[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(log2f, vector, result)
  }

  def log2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(log2f, vector)
  }

  def log2(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    log2d(vector, result)
  }

  def log2(vector: Vector[Double]): Vector[Double] = {
    log2d(vector)
  }

  def log2d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(log2d, vector, result)
  }

  def log2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(log2d, vector)
  }

  def log2[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(log2f, matrix, result)
  }

  def log2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(log2f, matrix)
  }

  def log2(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    log2d(matrix, result)
  }

  def log2(matrix: Matrix[Double]): Matrix[Double] = {
    log2d(matrix)
  }

  def log2d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(log2d, matrix, result)
  }

  def log2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(log2d, matrix)
  }

  def max[T: ClassTag](vector: Vector[T], threshold: T, result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(max1, vector, threshold, result)
  }

  def max[T: ClassTag](vector: Vector[T], threshold: T): Vector[T] = {
    MathKernel.vector(max1, vector, threshold)
  }

  def max[T: ClassTag](matrix: Matrix[T], threshold: T, result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(max1, matrix, threshold, result)
  }

  def max[T: ClassTag](matrix: Matrix[T], threshold: T): Matrix[T] = {
    MathKernel.matrix(max1, matrix, threshold)
  }

  def max[T: ClassTag](vector1: Vector[T], vector2: Vector[T], result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(max, vector1, vector2, result)
  }

  def max[T: ClassTag](vector1: Vector[T], vector2: Vector[T]): Vector[T] = {
    MathKernel.vector(max, vector1, vector2)
  }

  def max[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(max, matrix1, matrix2, result)
  }

  def max[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[T] = {
    MathKernel.matrix(max, matrix1, matrix2)
  }

  def min[T: ClassTag](vector: Vector[T], threshold: T, result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(min1, vector, threshold, result)
  }

  def min[T: ClassTag](vector: Vector[T], threshold: T): Vector[T] = {
    MathKernel.vector(min1, vector, threshold)
  }

  def min[T: ClassTag](matrix: Matrix[T], threshold: T, result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(min1, matrix, threshold, result)
  }

  def min[T: ClassTag](matrix: Matrix[T], threshold: T): Matrix[T] = {
    MathKernel.matrix(min1, matrix, threshold)
  }

  def min[T: ClassTag](vector1: Vector[T], vector2: Vector[T], result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(min, vector1, vector2, result)
  }

  def min[T: ClassTag](vector1: Vector[T], vector2: Vector[T]): Vector[T] = {
    MathKernel.vector(min, vector1, vector2)
  }

  def min[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(min, matrix1, matrix2, result)
  }

  def min[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[T] = {
    MathKernel.matrix(min, matrix1, matrix2)
  }

  def pow[T: ClassTag](vector: Vector[T], power: Float, result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(powf, vector, power, result)
  }

  def pow[T: ClassTag](vector: Vector[T], power: Float): Vector[Float] = {
    MathKernel.vector(powf, vector, power)
  }

  def pow(vector: Vector[Double], power: Double, result: Vector[Double]): Vector[Double] = {
    powd(vector, power, result)
  }

  def pow(vector: Vector[Double], power: Double): Vector[Double] = {
    powd(vector, power)
  }

  def powd[T: ClassTag](vector: Vector[T], power: Double, result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(powd, vector, power, result)
  }

  def powd[T: ClassTag](vector: Vector[T], power: Double): Vector[Double] = {
    MathKernel.vector(powd, vector, power)
  }

  def pow[T: ClassTag](matrix: Matrix[T], power: Float, result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(powf, matrix, power, result)
  }

  def pow[T: ClassTag](matrix: Matrix[T], power: Float): Matrix[Float] = {
    MathKernel.matrix(powf, matrix, power)
  }

  def pow(matrix: Matrix[Double], power: Double, result: Matrix[Double]): Matrix[Double] = {
    powd(matrix, power, result)
  }

  def pow(matrix: Matrix[Double], power: Double): Matrix[Double] = {
    powd(matrix, power)
  }

  def powd[T: ClassTag](matrix: Matrix[T], power: Double, result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(powd, matrix, power: Double, result)
  }

  def powd[T: ClassTag](matrix: Matrix[T], power: Double): Matrix[Double] = {
    MathKernel.matrix(powd, matrix, power)
  }

  def pow2[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(pow2f, vector, result)
  }

  def pow2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(pow2f, vector)
  }

  def pow2(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    pow2d(vector, result)
  }

  def pow2(vector: Vector[Double]): Vector[Double] = {
    pow2d(vector)
  }

  def pow2d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(pow2d, vector, result)
  }

  def pow2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(pow2d, vector)
  }

  def pow2[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(pow2f, matrix, result)
  }

  def pow2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(pow2f, matrix)
  }

  def pow2(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    pow2d(matrix, result)
  }

  def pow2(matrix: Matrix[Double]): Matrix[Double] = {
    pow2d(matrix)
  }

  def pow2d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(pow2d, matrix, result)
  }

  def pow2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(pow2d, matrix)
  }

  def relu[T: ClassTag](vector: Vector[T], result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(relu, vector, result)
  }

  def relu[T: ClassTag](vector: Vector[T]): Vector[T] = {
    MathKernel.vector(relu, vector)
  }

  def relu[T: ClassTag](matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(relu, matrix, result)
  }

  def relu[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    MathKernel.matrix(relu, matrix)
  }

  def round[T: ClassTag](vector: Vector[Float], result: Vector[Int]): Vector[Int] = {
    MathKernel.vector_r(roundf, vector, result)
  }

  def round[T: ClassTag](vector: Vector[Float]): Vector[Int] = {
    MathKernel.vector(roundf, vector)
  }

  def roundd[T: ClassTag](vector: Vector[Double], result: Vector[Long]): Vector[Long] = {
    MathKernel.vector_r(roundd, vector, result)
  }

  def roundd[T: ClassTag](vector: Vector[Double]): Vector[Long] = {
    MathKernel.vector(roundd, vector)
  }

  def round[T: ClassTag](matrix: Matrix[Float], result: Matrix[Int]): Matrix[Int] = {
    MathKernel.matrix_r(roundf, matrix, result)
  }

  def round[T: ClassTag](matrix: Matrix[Float]): Matrix[Int] = {
    MathKernel.matrix(roundf, matrix)
  }

  def roundd[T: ClassTag](matrix: Matrix[Double], result: Matrix[Long]): Matrix[Long] = {
    MathKernel.matrix_r(roundd, matrix, result)
  }

  def roundd[T: ClassTag](matrix: Matrix[Double]): Matrix[Long] = {
    MathKernel.matrix(roundd, matrix)
  }

  def sigmoid[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(sigmoidf, vector, result)
  }

  def sigmoid[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(sigmoidf, vector)
  }

  def sigmoid(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    sigmoidd(vector, result)
  }

  def sigmoid(vector: Vector[Double]): Vector[Double] = {
    sigmoidd(vector)
  }

  def sigmoidd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(sigmoidd, vector, result)
  }

  def sigmoidd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(sigmoidd, vector)
  }

  def sigmoid[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(sigmoidf, matrix, result)
  }

  def sigmoid[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(sigmoidf, matrix)
  }

  def sigmoid(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    sigmoidd(matrix, result)
  }

  def sigmoid(matrix: Matrix[Double]): Matrix[Double] = {
    sigmoidd(matrix)
  }

  def sigmoidd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(sigmoidd, matrix, result)
  }

  def sigmoidd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(sigmoidd, matrix)
  }

  def sign[T: ClassTag](vector: Vector[T], result: Vector[T]): Vector[T] = {
    MathKernel.vector_r(sign, vector, result)
  }

  def sign[T: ClassTag](vector: Vector[T]): Vector[T] = {
    MathKernel.vector(sign, vector)
  }

  def sign[T: ClassTag](matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
    MathKernel.matrix_r(sign, matrix, result)
  }

  def sign[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    MathKernel.matrix(sign, matrix)
  }

  def sin[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(sinf, vector, result)
  }

  def sin[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(sinf, vector)
  }

  def sin(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    sind(vector, result)
  }

  def sin(vector: Vector[Double]): Vector[Double] = {
    sind(vector)
  }

  def sind[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(sind, vector, result)
  }

  def sind[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(sind, vector)
  }

  def sin[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(sinf, matrix, result)
  }

  def sin[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(sinf, matrix)
  }

  def sin(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    sind(matrix, result)
  }

  def sin(matrix: Matrix[Double]): Matrix[Double] = {
    sind(matrix)
  }

  def sind[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(sind, matrix, result)
  }

  def sind[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(sind, matrix)
  }

  def sinh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(sinhf, vector, result)
  }

  def sinh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(sinhf, vector)
  }

  def sinh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    sinhd(vector, result)
  }

  def sinh(vector: Vector[Double]): Vector[Double] = {
    sinhd(vector)
  }

  def sinhd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(sinhd, vector, result)
  }

  def sinhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(sinhd, vector)
  }

  def sinh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(sinhf, matrix, result)
  }

  def sinh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(sinhf, matrix)
  }

  def sinh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    sinhd(matrix, result)
  }

  def sinh(matrix: Matrix[Double]): Matrix[Double] = {
    sinhd(matrix)
  }

  def sinhd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(sinhd, matrix, result)
  }

  def sinhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(sinhd, matrix)
  }

  def sqrt[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(sqrtf, vector, result)
  }

  def sqrt[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(sqrtf, vector)
  }

  def sqrt(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    sqrtd(vector, result)
  }

  def sqrt(vector: Vector[Double]): Vector[Double] = {
    sqrtd(vector)
  }

  def sqrtd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(sqrtd, vector, result)
  }

  def sqrtd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(sqrtd, vector)
  }

  def sqrt[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(sqrtf, matrix, result)
  }

  def sqrt[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(sqrtf, matrix)
  }

  def sqrt(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    sqrtd(matrix, result)
  }

  def sqrt(matrix: Matrix[Double]): Matrix[Double] = {
    sqrtd(matrix)
  }

  def sqrtd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(sqrtd, matrix, result)
  }

  def sqrtd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(sqrtd, matrix)
  }

  def tan[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(tanf, vector, result)
  }

  def tan[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(tanf, vector)
  }

  def tan(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    tand(vector, result)
  }

  def tan(vector: Vector[Double]): Vector[Double] = {
    tand(vector)
  }

  def tand[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(tand, vector, result)
  }

  def tand[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(tand, vector)
  }

  def tan[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(tanf, matrix, result)
  }

  def tan[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(tanf, matrix)
  }

  def tan(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    tand(matrix, result)
  }

  def tan(matrix: Matrix[Double]): Matrix[Double] = {
    tand(matrix)
  }

  def tand[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(tand, matrix, result)
  }

  def tand[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(tand, matrix)
  }

  def tanh[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
    MathKernel.vector_r(tanhf, vector, result)
  }

  def tanh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    MathKernel.vector(tanhf, vector)
  }

  def tanh(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
    tanhd(vector, result)
  }

  def tanh(vector: Vector[Double]): Vector[Double] = {
    tanhd(vector)
  }

  def tanhd[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
    MathKernel.vector_r(tanhd, vector, result)
  }

  def tanhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    MathKernel.vector(tanhd, vector)
  }

  def tanh[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
    MathKernel.matrix_r(tanhf, matrix, result)
  }

  def tanh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    MathKernel.matrix(tanhf, matrix)
  }

  def tanh(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
    tanhd(matrix, result)
  }

  def tanh(matrix: Matrix[Double]): Matrix[Double] = {
    tanhd(matrix)
  }

  def tanhd[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
    MathKernel.matrix_r(tanhd, matrix, result)
  }

  def tanhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    MathKernel.matrix(tanhd, matrix)
  }

  private lazy val abs = new KernelTemplate(new MathKernel("abs"))
  private lazy val acosf = new KernelTemplate(new MathKernel("acosf"))
  private lazy val acosd = new KernelTemplate(new MathKernel("acosd"))
  private lazy val acoshf = new KernelTemplate(new MathKernel("acoshf"))
  private lazy val acoshd = new KernelTemplate(new MathKernel("acoshd"))
  private lazy val asinf = new KernelTemplate(new MathKernel("asinf"))
  private lazy val asind = new KernelTemplate(new MathKernel("asind"))
  private lazy val asinhf = new KernelTemplate(new MathKernel("asinhf"))
  private lazy val asinhd = new KernelTemplate(new MathKernel("asinhd"))
  private lazy val atanf = new KernelTemplate(new MathKernel("atanf"))
  private lazy val atand = new KernelTemplate(new MathKernel("atand"))
  private lazy val atanhf = new KernelTemplate(new MathKernel("atanhf"))
  private lazy val atanhd = new KernelTemplate(new MathKernel("atanhd"))
  private lazy val cbrtf = new KernelTemplate(new MathKernel("cbrtf"))
  private lazy val cbrtd = new KernelTemplate(new MathKernel("cbrtd"))
  private lazy val ceilf = new KernelTemplate(new MathKernel("ceilf"))
  private lazy val ceild = new KernelTemplate(new MathKernel("ceild"))
  private lazy val cosf = new KernelTemplate(new MathKernel("cosf"))
  private lazy val cosd = new KernelTemplate(new MathKernel("cosd"))
  private lazy val coshf = new KernelTemplate(new MathKernel("coshf"))
  private lazy val coshd = new KernelTemplate(new MathKernel("coshd"))
  private lazy val expf = new KernelTemplate(new MathKernel("expf"))
  private lazy val expd = new KernelTemplate(new MathKernel("expd"))
  private lazy val exp10f = new KernelTemplate(new MathKernel("exp10f"))
  private lazy val exp10d = new KernelTemplate(new MathKernel("exp10d"))
  private lazy val exp2f = new KernelTemplate(new MathKernel("exp2f"))
  private lazy val exp2d = new KernelTemplate(new MathKernel("exp2d"))
  private lazy val floorf = new KernelTemplate(new MathKernel("floorf"))
  private lazy val floord = new KernelTemplate(new MathKernel("floord"))
  private lazy val max = new KernelTemplate(new MathKernel("max"))
  private lazy val max1 = new KernelTemplate(new MathKernel("max1"))
  private lazy val min = new KernelTemplate(new MathKernel("min"))
  private lazy val min1 = new KernelTemplate(new MathKernel("min1"))
  private lazy val lnf = new KernelTemplate(new MathKernel("lnf"))
  private lazy val lnd = new KernelTemplate(new MathKernel("lnd"))
  private lazy val logf = new KernelTemplate(new MathKernel("logf"))
  private lazy val logd = new KernelTemplate(new MathKernel("logd"))
  private lazy val log10f = new KernelTemplate(new MathKernel("log10f"))
  private lazy val log10d = new KernelTemplate(new MathKernel("log10d"))
  private lazy val log2f = new KernelTemplate(new MathKernel("log2f"))
  private lazy val log2d = new KernelTemplate(new MathKernel("log2d"))
  private lazy val powf = new KernelTemplate(new MathKernel("powf"))
  private lazy val powd = new KernelTemplate(new MathKernel("powd"))
  private lazy val pow2f = new KernelTemplate(new MathKernel("pow2f"))
  private lazy val pow2d = new KernelTemplate(new MathKernel("pow2d"))
  private lazy val relu = new KernelTemplate(new MathKernel("relu"))
  private lazy val roundf = new MathKernel("round_f")
  private lazy val roundd = new MathKernel("round_d")
  private lazy val sigmoidf = new KernelTemplate(new MathKernel("sigmoidf"))
  private lazy val sigmoidd = new KernelTemplate(new MathKernel("sigmoidd"))
  private lazy val sign = new KernelTemplate(new MathKernel("sign"))
  private lazy val sinf = new KernelTemplate(new MathKernel("sinf"))
  private lazy val sind = new KernelTemplate(new MathKernel("sind"))
  private lazy val sinhf = new KernelTemplate(new MathKernel("sinhf"))
  private lazy val sinhd = new KernelTemplate(new MathKernel("sinhd"))
  private lazy val sqrtf = new KernelTemplate(new MathKernel("sqrtf"))
  private lazy val sqrtd = new KernelTemplate(new MathKernel("sqrtd"))
  private lazy val tanf = new KernelTemplate(new MathKernel("tanf"))
  private lazy val tand = new KernelTemplate(new MathKernel("tand"))
  private lazy val tanhf = new KernelTemplate(new MathKernel("tanhf"))
  private lazy val tanhd = new KernelTemplate(new MathKernel("tanhd"))

}
