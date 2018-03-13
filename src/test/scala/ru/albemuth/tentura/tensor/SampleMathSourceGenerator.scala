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
      |  lazy val lnf = new KernelTemplate(new MathKernel("lnf"))
      |  lazy val lnd = new KernelTemplate(new MathKernel("lnd"))
      |  lazy val logf = new KernelTemplate(new MathKernel("logf"))
      |  lazy val logd = new KernelTemplate(new MathKernel("logd"))
      |  lazy val log10f = new KernelTemplate(new MathKernel("log10f"))
      |  lazy val log10d = new KernelTemplate(new MathKernel("log10d"))
      |  lazy val log2f = new KernelTemplate(new MathKernel("log2f"))
      |  lazy val log2d = new KernelTemplate(new MathKernel("log2d"))
      |  lazy val max = new KernelTemplate(new MathKernel("max"))
      |  lazy val min = new KernelTemplate(new MathKernel("min"))
      |  lazy val powf = new KernelTemplate(new MathKernel("powf"))
      |  lazy val powd = new KernelTemplate(new MathKernel("powd"))
      |  lazy val pow2f = new KernelTemplate(new MathKernel("pow2f"))
      |  lazy val pow2d = new KernelTemplate(new MathKernel("pow2d"))
      |  lazy val reluf = new KernelTemplate(new MathKernel("reluf"))
      |  lazy val relud = new KernelTemplate(new MathKernel("relud"))
      |  lazy val roundf = new KernelTemplate(new MathKernel("roundf"))
      |  lazy val roundd = new KernelTemplate(new MathKernel("roundd"))
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
    if (kernel == "abs" || kernel == "sign") {
      val source =
        s"""
          |  def ${kernel}[T: ClassTag](vector: Vector[T], result: Vector[T]): Vector[T] = {
          |    MathKernel.vector_r(${kernel}, vector, result)
          |  }
          |
          |  def ${kernel}[T: ClassTag](vector: Vector[T]): Vector[T] = {
          |    MathKernel.vector(${kernel}, vector)
          |  }
          |
          |  def ${kernel}[T: ClassTag](matrix: Matrix[T], result: Matrix[T]): Matrix[T] = {
          |    MathKernel.matrix_r(${kernel}, matrix, result)
          |  }
          |
          |  def ${kernel}[T: ClassTag](matrix: Matrix[T]): Matrix[T] = {
          |    MathKernel.matrix(${kernel}, matrix)
          |  }
        """.stripMargin
      print(source)
    } else if (kernel == "max" || kernel == "min") {
      val source =
        s"""
          |  def ${kernel}[T: ClassTag](vector1: Vector[T], vector2: Vector[T], result: Vector[T]): Vector[T] = {
          |    MathKernel.vector_r(${kernel}, vector1, vector2, result)
          |  }
          |
          |  def ${kernel}[T: ClassTag](vector1: Vector[T], vector2: Vector[T]): Vector[T] = {
          |    MathKernel.vector(${kernel}, vector1, vector2)
          |  }
          |
          |  def ${kernel}[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T], result: Matrix[T]): Matrix[T] = {
          |    MathKernel.matrix_r(${kernel}, matrix1, matrix2, result)
          |  }
          |
          |  def ${kernel}[T: ClassTag](matrix1: Matrix[T], matrix2: Matrix[T]): Matrix[T] = {
          |    MathKernel.matrix(${kernel}, matrix1, matrix2)
          |  }
        """.stripMargin
      print(source)
    } else if (kernel == "logf") {
      val source =
        s"""
          |  def log[T: ClassTag](vector: Vector[T], base: Float, result: Vector[Float]): Vector[Float] = {
          |    MathKernel.vector_r(logf, vector, base, result)
          |  }
          |
          |  def log[T: ClassTag](vector: Vector[T], base: Float): Vector[Float] = {
          |    MathKernel.vector(logf, vector, base)
          |  }
          |
          |  def log(vector: Vector[Double], base: Double, result: Vector[Double]): Vector[Double] = {
          |    logd(vector, base, result)
          |  }
          |
          |  def log(vector: Vector[Double], base: Double): Vector[Double] = {
          |    logd(vector, base)
          |  }
          |
          |  def logd[T: ClassTag](vector: Vector[T], base: Double, result: Vector[Double]): Vector[Double] = {
          |    MathKernel.vector_r(logd, vector, base, result)
          |  }
          |
          |  def logd[T: ClassTag](vector: Vector[T], base: Double): Vector[Double] = {
          |    MathKernel.vector(logd, vector, base)
          |  }
          |
          |  def log[T: ClassTag](matrix: Matrix[T], base: Float, result: Matrix[Float]): Matrix[Float] = {
          |    MathKernel.matrix_r(logf, matrix, base, result)
          |  }
          |
          |  def log[T: ClassTag](matrix: Matrix[T], base: Float): Matrix[Float] = {
          |    MathKernel.matrix(logf, matrix, base)
          |  }
          |
          |  def log(matrix: Matrix[Double], base: Double, result: Matrix[Double]): Matrix[Double] = {
          |    logd(matrix, base, result)
          |  }
          |
          |  def log(matrix: Matrix[Double], base: Double): Matrix[Double] = {
          |    logd(matrix, base)
          |  }
          |
          |  def logd[T: ClassTag](matrix: Matrix[T], base: Double, result: Matrix[Double]): Matrix[Double] = {
          |    MathKernel.matrix_r(logd, matrix, base, result)
          |  }
          |
          |  def logd[T: ClassTag](matrix: Matrix[T], base: Double): Matrix[Double] = {
          |    MathKernel.matrix(logd, matrix, base)
          |  }
        """.stripMargin
      print(source)
    } else if (kernel == "powf") {
      val source =
        s"""
          |  def pow[T: ClassTag](vector: Vector[T], power: Float, result: Vector[Float]): Vector[Float] = {
          |    MathKernel.vector_r(powf, vector, power, result)
          |  }
          |
          |  def pow[T: ClassTag](vector: Vector[T], power: Float): Vector[Float] = {
          |    MathKernel.vector(powf, vector, power)
          |  }
          |
          |  def pow(vector: Vector[Double], power: Double, result: Vector[Double]): Vector[Double] = {
          |    powd(vector, power, result)
          |  }
          |
          |  def pow(vector: Vector[Double], power: Double): Vector[Double] = {
          |    powd(vector, power)
          |  }
          |
          |  def powd[T: ClassTag](vector: Vector[T], power: Double, result: Vector[Double]): Vector[Double] = {
          |    MathKernel.vector_r(powd, vector, power, result)
          |  }
          |
          |  def powd[T: ClassTag](vector: Vector[T], power: Double): Vector[Double] = {
          |    MathKernel.vector(powd, vector, power)
          |  }
          |
          |  def pow[T: ClassTag](matrix: Matrix[T], power: Float, result: Matrix[Float]): Matrix[Float] = {
          |    MathKernel.matrix_r(powf, matrix, power, result)
          |  }
          |
          |  def pow[T: ClassTag](matrix: Matrix[T], power: Float): Matrix[Float] = {
          |    MathKernel.matrix(powf, matrix, power)
          |  }
          |
          |  def pow(matrix: Matrix[Double], power: Double, result: Matrix[Double]): Matrix[Double] = {
          |    powd(matrix, power, result)
          |  }
          |
          |  def pow(matrix: Matrix[Double], power: Double): Matrix[Double] = {
          |    powd(matrix, power)
          |  }
          |
          |  def powd[T: ClassTag](matrix: Matrix[T], power: Double, result: Matrix[Double]): Matrix[Double] = {
          |    MathKernel.matrix_r(powd, matrix, power: Double, result)
          |  }
          |
          |  def powd[T: ClassTag](matrix: Matrix[T], power: Double): Matrix[Double] = {
          |    MathKernel.matrix(powd, matrix, power)
          |  }
        """.stripMargin
      print(source)
    } else if (kernel == "roundf") {
      val source =
        s"""
          |  def round[T: ClassTag](vector: Vector[Float], result: Vector[Int]): Vector[Int] = {
          |    MathKernel.vector_r(roundf, vector, result)
          |  }
          |
          |  def round[T: ClassTag](vector: Vector[Float]): Vector[Int] = {
          |    MathKernel.vector(roundf, vector)
          |  }
          |
          |  def roundd[T: ClassTag](vector: Vector[Double], result: Vector[Long]): Vector[Long] = {
          |    MathKernel.vector_r(roundd, vector, result)
          |  }
          |
          |  def roundd[T: ClassTag](vector: Vector[Double]): Vector[Long] = {
          |    MathKernel.vector(roundd, vector)
          |  }
          |
          |  def round[T: ClassTag](matrix: Matrix[Float], result: Matrix[Int]): Matrix[Int] = {
          |    MathKernel.matrix_r(roundf, matrix, result)
          |  }
          |
          |  def round[T: ClassTag](matrix: Matrix[Float]): Matrix[Int] = {
          |    MathKernel.matrix(roundf, matrix)
          |  }
          |
          |  def roundd[T: ClassTag](matrix: Matrix[Double], result: Matrix[Long]): Matrix[Long] = {
          |    MathKernel.matrix_r(roundd, matrix, result)
          |  }
          |
          |  def roundd[T: ClassTag](matrix: Matrix[Double]): Matrix[Long] = {
          |    MathKernel.matrix(roundd, matrix)
          |  }
        """.stripMargin
      print(source)
    } else if (kernel.endsWith("f")) {
      val function = kernel.substring(0, kernel.length - 1)
      val source =
        s"""
          |  def $function[T: ClassTag](vector: Vector[T], result: Vector[Float]): Vector[Float] = {
          |    MathKernel.vector_r(${function}f, vector, result)
          |  }
          |
          |  def $function[T: ClassTag](vector: Vector[T]): Vector[Float] = {
          |    MathKernel.vector(${function}f, vector)
          |  }
          |
          |  def $function(vector: Vector[Double], result: Vector[Double]): Vector[Double] = {
          |    ${function}d(vector, result)
          |  }
          |
          |  def $function(vector: Vector[Double]): Vector[Double] = {
          |    ${function}d(vector)
          |  }
          |
          |  def ${function}d[T: ClassTag](vector: Vector[T], result: Vector[Double]): Vector[Double] = {
          |    MathKernel.vector_r(${function}d, vector, result)
          |  }
          |
          |  def ${function}d[T: ClassTag](vector: Vector[T]): Vector[Double] = {
          |    MathKernel.vector(${function}d, vector)
          |  }
          |
          |  def $function[T: ClassTag](matrix: Matrix[T], result: Matrix[Float]): Matrix[Float] = {
          |    MathKernel.matrix_r(${function}f, matrix, result)
          |  }
          |
          |  def $function[T: ClassTag](matrix: Matrix[T]): Matrix[Float] = {
          |    MathKernel.matrix(${function}f, matrix)
          |  }
          |
          |  def $function(matrix: Matrix[Double], result: Matrix[Double]): Matrix[Double] = {
          |    ${function}d(matrix, result)
          |  }
          |
          |  def $function(matrix: Matrix[Double]): Matrix[Double] = {
          |    ${function}d(matrix)
          |  }
          |
          |  def ${function}d[T: ClassTag](matrix: Matrix[T], result: Matrix[Double]): Matrix[Double] = {
          |    MathKernel.matrix_r(${function}d, matrix, result)
          |  }
          |
          |  def ${function}d[T: ClassTag](matrix: Matrix[T]): Matrix[Double] = {
          |    MathKernel.matrix(${function}d, matrix)
          |  }
        """.stripMargin
      print(source)
    }
  }

}
