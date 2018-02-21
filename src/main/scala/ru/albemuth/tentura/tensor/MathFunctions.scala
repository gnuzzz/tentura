package ru.albemuth.tentura.tensor

import ru.albemuth.tentura.kernel.KernelTemplate
import ru.albemuth.tentura.tensor.kernel.math.MathKernel
import ru.albemuth.tentura.tensor.kernel.math.MathKernel.{vectort, matrixt}

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object MathFunctions {

  def abs[T: ClassTag](vector: Vector[T]): Vector[T] = {
    vectort(abs, vector)
  }

  def abs[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
    matrixt(abs, matrix)
  }

  def acos[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(acosf, vector)
  }

  def acos(vector: Vector[Double]): Vector[Double] = {
    acosd(vector)
  }

  def acosd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(acosd, vector)
  }

  def acos[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(acosf, matrix)
  }

  def acos(matrix: Matrix[Double]): Matrix[Double] = {
    acosd(matrix)
  }

  def acosd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(acosd, matrix)
  }

  def acosh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(acoshf, vector)
  }

  def acosh(vector: Vector[Double]): Vector[Double] = {
    acoshd(vector)
  }

  def acoshd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(acoshd, vector)
  }

  def acosh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(acoshf, matrix)
  }

  def acosh(matrix: Matrix[Double]): Matrix[Double] = {
    acoshd(matrix)
  }

  def acoshd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(acoshd, matrix)
  }

  def asin[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(asinf, vector)
  }

  def asin(vector: Vector[Double]): Vector[Double] = {
    asind(vector)
  }

  def asind[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(asind, vector)
  }

  def asin[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(asinf, matrix)
  }

  def asin(matrix: Matrix[Double]): Matrix[Double] = {
    asind(matrix)
  }

  def asind[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(asind, matrix)
  }

  def asinh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(asinhf, vector)
  }

  def asinh(vector: Vector[Double]): Vector[Double] = {
    asinhd(vector)
  }

  def asinhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(asinhd, vector)
  }

  def asinh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(asinhf, matrix)
  }

  def asinh(matrix: Matrix[Double]): Matrix[Double] = {
    asinhd(matrix)
  }

  def asinhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(asinhd, matrix)
  }

  def atan[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(atanf, vector)
  }

  def atan(vector: Vector[Double]): Vector[Double] = {
    atand(vector)
  }

  def atand[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(atand, vector)
  }

  def atan[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(atanf, matrix)
  }

  def atan(matrix: Matrix[Double]): Matrix[Double] = {
    atand(matrix)
  }

  def atand[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(atand, matrix)
  }

  def atanh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(atanhf, vector)
  }

  def atanh(vector: Vector[Double]): Vector[Double] = {
    atanhd(vector)
  }

  def atanhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(atanhd, vector)
  }

  def atanh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(atanhf, matrix)
  }

  def atanh(matrix: Matrix[Double]): Matrix[Double] = {
    atanhd(matrix)
  }

  def atanhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(atanhd, matrix)
  }

  def cbrt[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(cbrtf, vector)
  }

  def cbrt(vector: Vector[Double]): Vector[Double] = {
    cbrtd(vector)
  }

  def cbrtd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(cbrtd, vector)
  }

  def cbrt[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(cbrtf, matrix)
  }

  def cbrt(matrix: Matrix[Double]): Matrix[Double] = {
    cbrtd(matrix)
  }

  def cbrtd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(cbrtd, matrix)
  }

  def ceil[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(ceilf, vector)
  }

  def ceil(vector: Vector[Double]): Vector[Double] = {
    ceild(vector)
  }

  def ceild[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(ceild, vector)
  }

  def ceil[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(ceilf, matrix)
  }

  def ceil(matrix: Matrix[Double]): Matrix[Double] = {
    ceild(matrix)
  }

  def ceild[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(ceild, matrix)
  }

  def cos[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(cosf, vector)
  }

  def cos(vector: Vector[Double]): Vector[Double] = {
    cosd(vector)
  }

  def cosd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(cosd, vector)
  }

  def cos[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(cosf, matrix)
  }

  def cos(matrix: Matrix[Double]): Matrix[Double] = {
    cosd(matrix)
  }

  def cosd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(cosd, matrix)
  }

  def cosh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(coshf, vector)
  }

  def cosh(vector: Vector[Double]): Vector[Double] = {
    coshd(vector)
  }

  def coshd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(coshd, vector)
  }

  def cosh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(coshf, matrix)
  }

  def cosh(matrix: Matrix[Double]): Matrix[Double] = {
    coshd(matrix)
  }

  def coshd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(coshd, matrix)
  }

  def exp[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(expf, vector)
  }

  def exp(vector: Vector[Double]): Vector[Double] = {
    expd(vector)
  }

  def expd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(expd, vector)
  }

  def exp[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(expf, matrix)
  }

  def exp(matrix: Matrix[Double]): Matrix[Double] = {
    expd(matrix)
  }

  def expd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(expd, matrix)
  }

  def exp10[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(exp10f, vector)
  }

  def exp10(vector: Vector[Double]): Vector[Double] = {
    exp10d(vector)
  }

  def exp10d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(exp10d, vector)
  }

  def exp10[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(exp10f, matrix)
  }

  def exp10(matrix: Matrix[Double]): Matrix[Double] = {
    exp10d(matrix)
  }

  def exp10d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(exp10d, matrix)
  }

  def exp2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(exp2f, vector)
  }

  def exp2(vector: Vector[Double]): Vector[Double] = {
    exp2d(vector)
  }

  def exp2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(exp2d, vector)
  }

  def exp2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(exp2f, matrix)
  }

  def exp2(matrix: Matrix[Double]): Matrix[Double] = {
    exp2d(matrix)
  }

  def exp2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(exp2d, matrix)
  }

  def floor[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(floorf, vector)
  }

  def floor(vector: Vector[Double]): Vector[Double] = {
    floord(vector)
  }

  def floord[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(floord, vector)
  }

  def floor[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(floorf, matrix)
  }

  def floor(matrix: Matrix[Double]): Matrix[Double] = {
    floord(matrix)
  }

  def floord[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(floord, matrix)
  }

  def ln[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(lnf, vector)
  }

  def ln(vector: Vector[Double]): Vector[Double] = {
    lnd(vector)
  }

  def lnd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(lnd, vector)
  }

  def ln[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(lnf, matrix)
  }

  def ln(matrix: Matrix[Double]): Matrix[Double] = {
    lnd(matrix)
  }

  def lnd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(lnd, matrix)
  }

  def log[T: ClassTag](vector: Vector[T], base: Float): Vector[Float] = {
    vectort(logf, vector, base)
  }

  def log(vector: Vector[Double], base: Double): Vector[Double] = {
    logd(vector, base)
  }

  def logd[T: ClassTag](vector: Vector[T], base: Double): Vector[Double] = {
    vectort(logd, vector, base)
  }

  def log[T: ClassTag](matrix: Matrix[T], base: Float): Matrix[Float] = {
    matrixt(logf, matrix, base)
  }

  def log(matrix: Matrix[Double], base: Double): Matrix[Double] = {
    logd(matrix, base)
  }

  def logd[T: ClassTag](matrix: Matrix[T], base: Double): Matrix[Double] = {
    matrixt(logd, matrix, base)
  }

  def log10[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(log10f, vector)
  }

  def log10(vector: Vector[Double]): Vector[Double] = {
    log10d(vector)
  }

  def log10d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(log10d, vector)
  }

  def log10[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(log10f, matrix)
  }

  def log10(matrix: Matrix[Double]): Matrix[Double] = {
    log10d(matrix)
  }

  def log10d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(log10d, matrix)
  }

  def log2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(log2f, vector)
  }

  def log2(vector: Vector[Double]): Vector[Double] = {
    log2d(vector)
  }

  def log2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(log2d, vector)
  }

  def log2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(log2f, matrix)
  }

  def log2(matrix: Matrix[Double]): Matrix[Double] = {
    log2d(matrix)
  }

  def log2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(log2d, matrix)
  }

  def max[T: ClassTag](vector1: Vector[T], vector2: Vector[T]): Vector[T] = {
    vectort(max, vector1, vector2)
  }

  def max[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[T] = {
    matrixt(max, matrix1, matrix2)
  }

  def min[T: ClassTag](vector1: Vector[T], vector2: Vector[T]): Vector[T] = {
    vectort(min, vector1, vector2)
  }

  def min[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[T] = {
    matrixt(min, matrix1, matrix2)
  }

  def pow[T: ClassTag](vector: Vector[T], power: Float): Vector[Float] = {
    vectort(powf, vector, power)
  }

  def pow(vector: Vector[Double], power: Double): Vector[Double] = {
    powd(vector, power)
  }

  def powd[T: ClassTag](vector: Vector[T], power: Double): Vector[Double] = {
    vectort(powd, vector, power)
  }

  def pow[T: ClassTag](matrix: Matrix[T], power: Float): Matrix[Float] = {
    matrixt(powf, matrix, power)
  }

  def pow(matrix: Matrix[Double], power: Double): Matrix[Double] = {
    powd(matrix, power)
  }

  def powd[T: ClassTag](matrix: Matrix[T], power: Double): Matrix[Double] = {
    matrixt(powd, matrix, power)
  }

  def pow2[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(pow2f, vector)
  }

  def pow2(vector: Vector[Double]): Vector[Double] = {
    pow2d(vector)
  }

  def pow2d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(pow2d, vector)
  }

  def pow2[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(pow2f, matrix)
  }

  def pow2(matrix: Matrix[Double]): Matrix[Double] = {
    pow2d(matrix)
  }

  def pow2d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(pow2d, matrix)
  }

  def relu[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(reluf, vector)
  }

  def relu(vector: Vector[Double]): Vector[Double] = {
    relud(vector)
  }

  def relud[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(relud, vector)
  }

  def relu[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(reluf, matrix)
  }

  def relu(matrix: Matrix[Double]): Matrix[Double] = {
    relud(matrix)
  }

  def relud[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(relud, matrix)
  }

  def round[T: ClassTag](vector: Vector[Float]): Vector[Int] = {
    vectort(roundf, vector)
  }

  def roundd[T: ClassTag](vector: Vector[Double]): Vector[Long] = {
    vectort(roundd, vector)
  }

  def round[T: ClassTag](matrix: Matrix[Float]): Matrix[Int] = {
    matrixt(roundf, matrix)
  }

  def roundd[T: ClassTag](matrix: Matrix[Double]): Matrix[Long] = {
    matrixt(roundd, matrix)
  }

  def sigmoid[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(sigmoidf, vector)
  }

  def sigmoid(vector: Vector[Double]): Vector[Double] = {
    sigmoidd(vector)
  }

  def sigmoidd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(sigmoidd, vector)
  }

  def sigmoid[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(sigmoidf, matrix)
  }

  def sigmoid(matrix: Matrix[Double]): Matrix[Double] = {
    sigmoidd(matrix)
  }

  def sigmoidd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(sigmoidd, matrix)
  }

  def sin[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(sinf, vector)
  }

  def sin(vector: Vector[Double]): Vector[Double] = {
    sind(vector)
  }

  def sind[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(sind, vector)
  }

  def sin[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(sinf, matrix)
  }

  def sin(matrix: Matrix[Double]): Matrix[Double] = {
    sind(matrix)
  }

  def sind[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(sind, matrix)
  }

  def sinh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(sinhf, vector)
  }

  def sinh(vector: Vector[Double]): Vector[Double] = {
    sinhd(vector)
  }

  def sinhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(sinhd, vector)
  }

  def sinh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(sinhf, matrix)
  }

  def sinh(matrix: Matrix[Double]): Matrix[Double] = {
    sinhd(matrix)
  }

  def sinhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(sinhd, matrix)
  }

  def sqrt[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(sqrtf, vector)
  }

  def sqrt(vector: Vector[Double]): Vector[Double] = {
    sqrtd(vector)
  }

  def sqrtd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(sqrtd, vector)
  }

  def sqrt[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(sqrtf, matrix)
  }

  def sqrt(matrix: Matrix[Double]): Matrix[Double] = {
    sqrtd(matrix)
  }

  def sqrtd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(sqrtd, matrix)
  }

  def tan[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(tanf, vector)
  }

  def tan(vector: Vector[Double]): Vector[Double] = {
    tand(vector)
  }

  def tand[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(tand, vector)
  }

  def tan[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(tanf, matrix)
  }

  def tan(matrix: Matrix[Double]): Matrix[Double] = {
    tand(matrix)
  }

  def tand[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(tand, matrix)
  }

  def tanh[T: ClassTag](vector: Vector[T]): Vector[Float] = {
    vectort(tanhf, vector)
  }

  def tanh(vector: Vector[Double]): Vector[Double] = {
    tanhd(vector)
  }

  def tanhd[T: ClassTag](vector: Vector[T]): Vector[Double] = {
    vectort(tanhd, vector)
  }

  def tanh[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
    matrixt(tanhf, matrix)
  }

  def tanh(matrix: Matrix[Double]): Matrix[Double] = {
    tanhd(matrix)
  }

  def tanhd[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
    matrixt(tanhd, matrix)
  }

  lazy val abs = new KernelTemplate(new MathKernel("abs"))
  lazy val acosf = new KernelTemplate(new MathKernel("acosf"))
  lazy val acosd = new KernelTemplate(new MathKernel("acosd"))
  lazy val acoshf = new KernelTemplate(new MathKernel("acoshf"))
  lazy val acoshd = new KernelTemplate(new MathKernel("acoshd"))
  lazy val asinf = new KernelTemplate(new MathKernel("asinf"))
  lazy val asind = new KernelTemplate(new MathKernel("asind"))
  lazy val asinhf = new KernelTemplate(new MathKernel("asinhf"))
  lazy val asinhd = new KernelTemplate(new MathKernel("asinhd"))
  lazy val atanf = new KernelTemplate(new MathKernel("atanf"))
  lazy val atand = new KernelTemplate(new MathKernel("atand"))
  lazy val atanhf = new KernelTemplate(new MathKernel("atanhf"))
  lazy val atanhd = new KernelTemplate(new MathKernel("atanhd"))
  lazy val cbrtf = new KernelTemplate(new MathKernel("cbrtf"))
  lazy val cbrtd = new KernelTemplate(new MathKernel("cbrtd"))
  lazy val ceilf = new KernelTemplate(new MathKernel("ceilf"))
  lazy val ceild = new KernelTemplate(new MathKernel("ceild"))
  lazy val cosf = new KernelTemplate(new MathKernel("cosf"))
  lazy val cosd = new KernelTemplate(new MathKernel("cosd"))
  lazy val coshf = new KernelTemplate(new MathKernel("coshf"))
  lazy val coshd = new KernelTemplate(new MathKernel("coshd"))
  lazy val expf = new KernelTemplate(new MathKernel("expf"))
  lazy val expd = new KernelTemplate(new MathKernel("expd"))
  lazy val exp10f = new KernelTemplate(new MathKernel("exp10f"))
  lazy val exp10d = new KernelTemplate(new MathKernel("exp10d"))
  lazy val exp2f = new KernelTemplate(new MathKernel("exp2f"))
  lazy val exp2d = new KernelTemplate(new MathKernel("exp2d"))
  lazy val floorf = new KernelTemplate(new MathKernel("floorf"))
  lazy val floord = new KernelTemplate(new MathKernel("floord"))
  lazy val max = new KernelTemplate(new MathKernel("max"))
  lazy val min = new KernelTemplate(new MathKernel("min"))
  lazy val lnf = new KernelTemplate(new MathKernel("lnf"))
  lazy val lnd = new KernelTemplate(new MathKernel("lnd"))
  lazy val logf = new KernelTemplate(new MathKernel("logf"))
  lazy val logd = new KernelTemplate(new MathKernel("logd"))
  lazy val log10f = new KernelTemplate(new MathKernel("log10f"))
  lazy val log10d = new KernelTemplate(new MathKernel("log10d"))
  lazy val log2f = new KernelTemplate(new MathKernel("log2f"))
  lazy val log2d = new KernelTemplate(new MathKernel("log2d"))
  lazy val powf = new KernelTemplate(new MathKernel("powf"))
  lazy val powd = new KernelTemplate(new MathKernel("powd"))
  lazy val pow2f = new KernelTemplate(new MathKernel("pow2f"))
  lazy val pow2d = new KernelTemplate(new MathKernel("pow2d"))
  lazy val reluf = new KernelTemplate(new MathKernel("reluf"))
  lazy val relud = new KernelTemplate(new MathKernel("relud"))
  lazy val roundf = new MathKernel("round_f")
  lazy val roundd = new MathKernel("round_d")
  lazy val sigmoidf = new KernelTemplate(new MathKernel("sigmoidf"))
  lazy val sigmoidd = new KernelTemplate(new MathKernel("sigmoidd"))
  lazy val sign = new KernelTemplate(new MathKernel("sign"))
  lazy val sinf = new KernelTemplate(new MathKernel("sinf"))
  lazy val sind = new KernelTemplate(new MathKernel("sind"))
  lazy val sinhf = new KernelTemplate(new MathKernel("sinhf"))
  lazy val sinhd = new KernelTemplate(new MathKernel("sinhd"))
  lazy val sqrtf = new KernelTemplate(new MathKernel("sqrtf"))
  lazy val sqrtd = new KernelTemplate(new MathKernel("sqrtd"))
  lazy val tanf = new KernelTemplate(new MathKernel("tanf"))
  lazy val tand = new KernelTemplate(new MathKernel("tand"))
  lazy val tanhf = new KernelTemplate(new MathKernel("tanhf"))
  lazy val tanhd = new KernelTemplate(new MathKernel("tanhd"))

}
