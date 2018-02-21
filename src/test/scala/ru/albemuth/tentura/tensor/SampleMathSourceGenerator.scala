package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object SampleMathSourceGenerator extends App {

  val kernelsSource =
    """lazy val abs = new KernelTemplate(new MathKernel("abs"))
      |  lazy val acosf = new KernelTemplate(new MathKernel("acosf"))
      |  lazy val acosd = new KernelTemplate(new MathKernel("acosd"))
      |  lazy val acoshf = new KernelTemplate(new MathKernel("acoshf"))
      |  lazy val acoshd = new KernelTemplate(new MathKernel("acoshd"))
      |  lazy val asinf = new KernelTemplate(new MathKernel("asinf"))
      |  lazy val asind = new KernelTemplate(new MathKernel("asind"))
      |  lazy val asinhf = new KernelTemplate(new MathKernel("asinhf"))
      |  lazy val asinhd = new KernelTemplate(new MathKernel("asinhd"))
      |  lazy val atanf = new KernelTemplate(new MathKernel("atanf"))
      |  lazy val atand = new KernelTemplate(new MathKernel("atand"))
      |  lazy val atanhf = new KernelTemplate(new MathKernel("atanhf"))
      |  lazy val atanhd = new KernelTemplate(new MathKernel("atanhd"))
      |  lazy val cbrtf = new KernelTemplate(new MathKernel("cbrtf"))
      |  lazy val cbrtd = new KernelTemplate(new MathKernel("cbrtd"))
      |  lazy val ceilf = new KernelTemplate(new MathKernel("ceilf"))
      |  lazy val ceild = new KernelTemplate(new MathKernel("ceild"))
      |  lazy val cosf = new KernelTemplate(new MathKernel("cosf"))
      |  lazy val cosd = new KernelTemplate(new MathKernel("cosd"))
      |  lazy val coshf = new KernelTemplate(new MathKernel("coshf"))
      |  lazy val coshd = new KernelTemplate(new MathKernel("coshd"))
      |  lazy val expf = new KernelTemplate(new MathKernel("expf"))
      |  lazy val expd = new KernelTemplate(new MathKernel("expd"))
      |  lazy val exp10f = new KernelTemplate(new MathKernel("exp10f"))
      |  lazy val exp10d = new KernelTemplate(new MathKernel("exp10d"))
      |  lazy val exp2f = new KernelTemplate(new MathKernel("exp2f"))
      |  lazy val exp2d = new KernelTemplate(new MathKernel("exp2d"))
      |  lazy val floorf = new KernelTemplate(new MathKernel("floorf"))
      |  lazy val floord = new KernelTemplate(new MathKernel("floord"))
      |  lazy val max = new KernelTemplate(new MathKernel("max"))
      |  lazy val min = new KernelTemplate(new MathKernel("min"))
      |  lazy val lnf = new KernelTemplate(new MathKernel("lnf"))
      |  lazy val lnd = new KernelTemplate(new MathKernel("lnd"))
      |  lazy val logf = new KernelTemplate(new MathKernel("logf"))
      |  lazy val logd = new KernelTemplate(new MathKernel("logd"))
      |  lazy val log10f = new KernelTemplate(new MathKernel("log10f"))
      |  lazy val log10d = new KernelTemplate(new MathKernel("log10d"))
      |  lazy val log2f = new KernelTemplate(new MathKernel("log2f"))
      |  lazy val log2d = new KernelTemplate(new MathKernel("log2d"))
      |  lazy val powf = new KernelTemplate(new MathKernel("powf"))
      |  lazy val powd = new KernelTemplate(new MathKernel("powd"))
      |  lazy val pow2f = new KernelTemplate(new MathKernel("pow2f"))
      |  lazy val pow2d = new KernelTemplate(new MathKernel("pow2d"))
      |  lazy val reluf = new KernelTemplate(new MathKernel("reluf"))
      |  lazy val relud = new KernelTemplate(new MathKernel("relud"))
      |  lazy val roundf = new MathKernel("roundf")
      |  lazy val roundd = new MathKernel("roundd")
      |  lazy val sigmoidf = new KernelTemplate(new MathKernel("sigmoidf"))
      |  lazy val sigmoidd = new KernelTemplate(new MathKernel("sigmoidd"))
      |  lazy val sign = new KernelTemplate(new MathKernel("sign"))
      |  lazy val sinf = new KernelTemplate(new MathKernel("sinf"))
      |  lazy val sind = new KernelTemplate(new MathKernel("sind"))
      |  lazy val sinhf = new KernelTemplate(new MathKernel("sinhf"))
      |  lazy val sinhd = new KernelTemplate(new MathKernel("sinhd"))
      |  lazy val sqrtf = new KernelTemplate(new MathKernel("sqrtf"))
      |  lazy val sqrtd = new KernelTemplate(new MathKernel("sqrtd"))
      |  lazy val tanf = new KernelTemplate(new MathKernel("tanf"))
      |  lazy val tand = new KernelTemplate(new MathKernel("tand"))
      |  lazy val tanhf = new KernelTemplate(new MathKernel("tanhf"))
      |  lazy val tanhd = new KernelTemplate(new MathKernel("tanhd"))""".stripMargin

  val kernelRegex = "lazy val (\\w+) = new KernelTemplate\\(new MathKernel\\(\"\\w+\"\\)\\)".r

  for (kernelMatcher <- kernelRegex.findAllMatchIn(kernelsSource)) {
    val kernel = kernelMatcher.group(1)
    if (kernel.endsWith("f")) {
      val function = kernel.substring(0, kernel.length - 1)
      val source =
        s"""
          |  def $function[T: ClassTag](vector: Vector[T]): Vector[Float] = {
          |    vectorf(${function}f, vector)
          |  }
          |
          |  def $function(vector: Vector[Double]): Vector[Double] = {
          |    ${function}d(vector)
          |  }
          |
          |  def ${function}d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
          |    vectord(${function}d, vector)
          |  }
          |
          |  def $function[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
          |    matrixf(${function}f, matrix)
          |  }
          |
          |  def $function(matrix: Matrix[Double]): Matrix[Double] = {
          |    ${function}d(matrix)
          |  }
          |
          |  def ${function}d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
          |    matrixd(${function}d, matrix)
          |  }
        """.stripMargin
      print(source)
    }
  }

}
