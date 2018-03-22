package ru.albemuth.tentura.tensor

import jcuda.jcudpp._

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
abstract class CudppOperation(val plan: CUDPPHandle) extends Operation {

  override def release(): Unit = {
    JCudpp.cudppDestroyPlan(plan)
  }

}

object CudppOperation {
  val cudpp = new CUDPPHandle
  JCudpp.cudppCreate(cudpp)

  def release(): Unit = {
    JCudpp.cudppDestroy(cudpp)
  }

  def cudppConfig(algorithm: Int, datatype: Int, op: Int, options: Int): CUDPPConfiguration = {
    val config = new CUDPPConfiguration
    config.algorithm = algorithm
    config.datatype = datatype
    config.op = op
    config.options = options
    config
  }

  def cudppPlan(config: CUDPPConfiguration, n: Int, rows: Int, rowPitch: Int): CUDPPHandle = {
    val plan = new CUDPPHandle
    JCudpp.cudppPlan(CudppOperation.cudpp, plan, config, n, rows, rowPitch)
    plan
  }

  def datatype[T: ClassTag](vector: Vector[T]): Int = {
    implicitly[ClassTag[T]].runtimeClass match {
      case b if b == classOf[Boolean] => CUDPPDatatype.CUDPP_DATATYPE_INVALID
      case b if b == classOf[Byte] => CUDPPDatatype.CUDPP_CHAR
      case c if c == classOf[Char] => CUDPPDatatype.CUDPP_SHORT
      case s if s == classOf[Short] => CUDPPDatatype.CUDPP_SHORT
      case i if i == classOf[Int] => CUDPPDatatype.CUDPP_INT
      case l if l == classOf[Long] => CUDPPDatatype.CUDPP_LONGLONG
      case f if f == classOf[Float] => CUDPPDatatype.CUDPP_FLOAT
      case d if d == classOf[Double] => CUDPPDatatype.CUDPP_DOUBLE
      case _ => CUDPPDatatype.CUDPP_DATATYPE_INVALID
    }
  }

}