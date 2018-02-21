package ru.albemuth.tentura.kernel

import jcuda.{Pointer, Sizeof}
import jcuda.driver._
import java.io._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait JCudaKernel {
  val function: CUfunction
}

object JCudaKernel {

  JCudaKernel.init()

  def init(): Unit = {
    JCudaDriver.setExceptionsEnabled(true)
    JCudaDriver.cuInit(0)
    val device = new CUdevice
    JCudaDriver.cuDeviceGet(device, 0)
    val context = new CUcontext
    JCudaDriver.cuCtxCreate(context, 0, device)
  }

  def loadKernel(fileName: String, classifier: String, functionName: String): CUfunction = {
    val module = new CUmodule
    JCudaDriver.cuModuleLoadData(module, getPtxImage(fileName, classifier))
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
    val ptxFileName = fileName + "_" + classifier + ".ptx"
    val ptxFile = new File("src/main/resources/" + ptxFileName)
//    if (ptxFile.exists) return ptxFile.getPath

    val cuFile: File = new File("src/main/resources/" + cuFileName)
    if (!cuFile.exists) throw new IOException("Input file not found: " + cuFileName)
    val modelString = "-m" + System.getProperty("sun.arch.data.model")
//    val command = "nvcc " + modelString + " -ptx " + cuFile.getPath + " -o " + ptxFileName
    val command = "nvcc " + modelString + " -ptx " + cuFile.getPath + " -o " + ptxFile.getPath

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

  def sizeOfItem[T](data: Array[T]): Int = {
    data.getClass.getComponentType match {
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
      case b if b == classOf[Boolean] => ??? //not supported
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

  def devicePtr[T](data: Array[T]): CUdeviceptr = {
    val devicePtr = new CUdeviceptr
    JCudaDriver.cuMemAlloc(devicePtr, data.length * sizeOfItem(data))
    devicePtr
  }

}