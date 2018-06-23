package ru.albemuth.tentura.kernel

import jcuda.{Pointer, Sizeof}
import jcuda.driver._
import java.io._

import ru.albemuth.jcuda.jcusegsort.Datatype

import scala.collection.mutable
import scala.io.Source
import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait JCudaKernel {
  val function: CUfunction
}

object JCudaKernel {

  private val modules = new mutable.HashMap[String, CUmodule]()

  val device = new CUdevice
  lazy val numberOfSMs: Int = {
    val value = Array.ofDim[Int](1)
    JCudaDriver.cuDeviceGetAttribute(value, CUdevice_attribute.CU_DEVICE_ATTRIBUTE_MULTIPROCESSOR_COUNT, device)
    value(0)
  }

  JCudaKernel.init()

  def init(): Unit = {
    JCudaDriver.setExceptionsEnabled(true)
    JCudaDriver.cuInit(0)
    JCudaDriver.cuDeviceGet(device, 0)
    val context = new CUcontext
    JCudaDriver.cuCtxCreate(context, 0, device)
  }

  def loadKernel(moduleName: String, classifier: String, functionName: String): CUfunction = {
    val module = modules.getOrElseUpdate(moduleName, {
      val m = new CUmodule
      JCudaDriver.cuModuleLoadData(m, getPtxImage(moduleName, classifier))
      m
    })
    val function = new CUfunction
    JCudaDriver.cuModuleGetFunction(function, module, functionName)
    function
  }

  def getPtxImage(fileName: String, classifier: String): Array[Byte] = {
    val ptxResourceStream = getPtxResourceInputStream(fileName, classifier)
    if (ptxResourceStream != null) {
      toByteArray(ptxResourceStream)
    } else {
      val ptxFileStream = getPtxFileInputStream(fileName, classifier)
      if (ptxFileStream != null) {
        toByteArray(ptxFileStream)
      } else {
        null
      }
    }
  }

  def getPtxResourceInputStream(fileName: String, classifier: String): InputStream = {
    getClass.getClassLoader.getResourceAsStream(fileName + "_" + classifier + ".ptx")
  }

  def getPtxFileInputStream(fileName: String, classifier: String): InputStream = {
    val ptxFileName = preparePtxFile(fileName, classifier)
    new FileInputStream(ptxFileName)
  }

  def loadPtxFile(in: InputStream): Array[Byte] = {
    toByteArray(in)
  }

  def preparePtxFile(fileName: String, classifier: String): String = {
    val cuFileName = "/" + fileName + ".cu"
    val processedCuFileName = "/" + fileName + "_processed.cu"
    val ptxFileName = fileName + "_" + classifier + ".ptx"
    val cuFile: File = new File("src/main/resources/" + cuFileName)
    if (!cuFile.exists) throw new FileNotFoundException("Input file not found: " + cuFileName)
    val processedCuFile: File = new File("src/main/resources/" + processedCuFileName)
    val ptxFile = new File("src/main/resources/" + ptxFileName)

    processTemplates(cuFile.getPath, processedCuFile.getPath)
    val modelString = "-m" + System.getProperty("sun.arch.data.model")
    val command = "nvcc " + modelString + " -ptx " + processedCuFile.getPath + " -arch=sm_35 --expt-relaxed-constexpr -o " + ptxFile.getPath

    val process = Runtime.getRuntime.exec(command)

    val errorMessage = new String(toByteArray(process.getErrorStream))
    val outputMessage = new String(toByteArray(process.getInputStream))
    try {
      val exitValue = process.waitFor
      if (exitValue != 0) {
        println("nvcc process exitValue " + exitValue)
        println("errorMessage:\n" + errorMessage)
        println("outputMessage:\n" + outputMessage)
        throw new IOException("Could not create .ptx file: " + errorMessage)
      }
    } catch {
      case e: InterruptedException =>
        Thread.currentThread.interrupt()
        throw new IOException("Interrupted while waiting for nvcc output", e)
    }

    ptxFile.getPath
  }

  def processTemplates(cuFileName: String, processedCuFileName: String): Unit = {
    val cuSource = Source.fromFile(cuFileName).mkString
    val functionRegex = "template<typename T>\\s+__device__\\s+void\\s+(\\w+)\\s*\\(([^)]*)\\)".r
    val paramsRegex = "(?:const\\s+)?(\\w+)\\s*([*&])?\\s*(\\w+)\\s*,?".r
    val types = List(("Boolean", "unsigned char"), ("Byte", "char"), ("Char", "unsigned short"), ("Short", "short"), ("Int", "int"), ("Long", "long long int"), ("Float", "float"), ("Double", "double"))
    val materializedTemplates = for (
      functionMatcher <- functionRegex.findAllMatchIn(cuSource);
      templateType <- types
    ) yield {
      val name = functionMatcher.group(1)
      val params = paramsRegex.findAllMatchIn(functionMatcher.group(2)).map(m => (m.group(1), m.group(2), m.group(3))).toList
      templateImpl(name, templateType._1, params, templateType._2)
    }
    val processedCuSource = cuSource + "\n" + materializedTemplates.filter(t => !cuSource.contains(t._1)).map(_._2).mkString("\n")
    val out = new PrintWriter(new File(processedCuFileName))
    out.print(processedCuSource)
    out.close()
  }

  def templateImpl(name: String, postfix: String, params: List[(String, String, String)], typeName: String): (String, String) = {
    (s"${name}_${postfix}",
    s"""
extern "C"
__global__ void ${name}_${postfix}(${formalParams(params, typeName)}) {
  ${name}<${typeName}>(${actualParams(params)});
}"""
      )
  }

  def formalParams(params: List[(String, String, String)], typeName: String): String = {
    params.map(formalParam(_, typeName)).mkString(", ")
  }

  def formalParam(param: (String, String, String), typeName: String): String = {
    if ("T".equals(param._1))
      typeName + (if (param._2 != null) param._2 else "") + " " + param._3
    else
      param._1 + (if (param._2 != null) param._2 else "") + " " + param._3
  }

  def actualParams(params: List[(String, String, String)]): String = {
    params.map(_._3).mkString(", ")
  }

  def toByteArray(inputStream: InputStream): Array[Byte] = {
    val baos = new ByteArrayOutputStream
    val buffer = new Array[Byte](8192)
    bufferedRead(inputStream, baos, buffer)
    baos.toByteArray
  }

  def bufferedRead(in: InputStream, out: OutputStream, buffer: Array[Byte]): Unit = {
    val read = in.read(buffer)
    if (read != -1) {
      out.write(buffer, 0, read)
      bufferedRead(in, out, buffer)
    }
  }

  def sizeOf[T: ClassTag](): Int = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => Sizeof.BYTE
      case b if b == classOf[Byte] => Sizeof.BYTE
      case c if c == classOf[Char] => Sizeof.CHAR
      case s if s == classOf[Short] => Sizeof.SHORT
      case i if i == classOf[Int] => Sizeof.INT
      case l if l == classOf[Long] => Sizeof.LONG
      case f if f == classOf[Float] => Sizeof.FLOAT
      case d if d == classOf[Double] => Sizeof.DOUBLE
      case _ => Sizeof.POINTER
    }
  }

  def pointer[T](data: Array[T]): Pointer = {
    data.getClass.getComponentType match {
      case b if b == classOf[Boolean] => Pointer.to(data.asInstanceOf[Array[Boolean]].map(if (_) 1.toByte else 0.toByte))
      case b if b == classOf[Byte] => Pointer.to(data.asInstanceOf[Array[Byte]])
      case c if c == classOf[Char] => Pointer.to(data.asInstanceOf[Array[Char]])
      case s if s == classOf[Short] => Pointer.to(data.asInstanceOf[Array[Short]])
      case i if i == classOf[Int] => Pointer.to(data.asInstanceOf[Array[Int]])
      case l if l == classOf[Long] => Pointer.to(data.asInstanceOf[Array[Long]])
      case f if f == classOf[Float] => Pointer.to(data.asInstanceOf[Array[Float]])
      case d if d == classOf[Double] => Pointer.to(data.asInstanceOf[Array[Double]])
      case _ => ??? //not supported
    }
  }

  def array[T: ClassTag](length: Int): Array[T] = {
    val tTag = implicitly[ClassTag[T]]
    tTag.newArray(length)
  }

  def devicePtr[T: ClassTag](dataLength: Long): CUdeviceptr = {
    val devicePtr = new CUdeviceptr
    JCudaDriver.cuMemAlloc(devicePtr, dataLength * sizeOf[T]())
    devicePtr
  }

  def datatype[T: ClassTag](): Datatype = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    clazz match {
      case b if b == classOf[Boolean] => Datatype.BOOLEAN
      case b if b == classOf[Byte] => Datatype.BYTE
      case c if c == classOf[Char] => Datatype.CHAR
      case s if s == classOf[Short] => Datatype.SHORT
      case i if i == classOf[Int] => Datatype.INT
      case l if l == classOf[Long] => Datatype.LONG
      case f if f == classOf[Float] => Datatype.FLOAT
      case d if d == classOf[Double] => Datatype.DOUBLE
      case _ => ??? //not supported
    }
  }

}