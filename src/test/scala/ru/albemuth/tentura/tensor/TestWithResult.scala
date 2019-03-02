package ru.albemuth.tentura.tensor

import org.scalatest.Assertions

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
trait TestWithResult extends Assertions with TestUtils {

  def testWithResultV_Vbl[T: ClassTag](v1: Vector[T], op: (Vector[T]) => Vector[Boolean], op_r: (Vector[T], Vector[Boolean]) => Vector[Boolean]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Boolean](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Boolean](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_Vc(v1: Vector[Char], op: (Vector[Char]) => Vector[Char], op_r: (Vector[Char], Vector[Char]) => Vector[Char]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Char](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Char](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_Vi[T: ClassTag](v1: Vector[T], op: (Vector[T]) => Vector[Int], op_r: (Vector[T], Vector[Int]) => Vector[Int]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Int](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Int](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_Vl[T: ClassTag](v1: Vector[T], op: (Vector[T]) => Vector[Long], op_r: (Vector[T], Vector[Long]) => Vector[Long]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Long](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Long](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_Vf(v1: Vector[Float], op: (Vector[Float]) => Vector[Float], op_r: (Vector[Float], Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_Vd(v1: Vector[Double], op: (Vector[Double]) => Vector[Double], op_r: (Vector[Double], Vector[Double]) => Vector[Double]): Unit = {
    val length = {
      val r1 = op(v1)
      val r2 = new Vector[Double](r1.length)
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Double](length)
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultV_S[V: ClassTag, R: ClassTag](v1: Vector[V], op: (Vector[V]) => Scalar[R], op_r: (Vector[V], Scalar[R]) => Scalar[R]): Unit = {
    {
      val r1 = op(v1)
      val r2 = new Scalar[R]()
      val r3 = op_r(v1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
    {
      val r2 = new Scalar[R]()
      val r3 = op_r(v1, r2)
      val r1 = op(v1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
  }

  def testWithResultVM_V(v1: Vector[Float], m2: Matrix[Float], op: (Vector[Float], Matrix[Float]) => Vector[Float], op_r: (Vector[Float], Matrix[Float], Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(v1, m2)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(v1, m2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(v1, m2, r2)
      val r1 = op(v1, m2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVM_M[V: ClassTag, M: ClassTag](v1: Vector[V], m2: Matrix[M], op: (Vector[V], Matrix[M]) => Matrix[Float], op_r: (Vector[V], Matrix[M], Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(v1, m2)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(v1, m2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(v1, m2, r2)
      val r1 = op(v1, m2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVV_M(v1: Vector[Float], v2: Vector[Float], op: (Vector[Float], Vector[Float]) => Matrix[Float], op_r: (Vector[Float], Vector[Float], Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(v1, v2)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(v1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(v1, v2, r2)
      val r1 = op(v1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVV_V[V1: ClassTag, V2: ClassTag](v1: Vector[V1], v2: Vector[V2], op: (Vector[V1], Vector[V2]) => Vector[Float], op_r: (Vector[V1], Vector[V2], Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(v1, v2)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(v1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(v1, v2, r2)
      val r1 = op(v1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVV_Vd[V1: ClassTag, V2: ClassTag](v1: Vector[V1], v2: Vector[V2], op: (Vector[V1], Vector[V2]) => Vector[Double], op_r: (Vector[V1], Vector[V2], Vector[Double]) => Vector[Double]): Unit = {
    val length = {
      val r1 = op(v1, v2)
      val r2 = new Vector[Double](r1.length)
      val r3 = op_r(v1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Double](length)
      val r3 = op_r(v1, v2, r2)
      val r1 = op(v1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVV_S(v1: Vector[Float], v2: Vector[Float], op: (Vector[Float], Vector[Float]) => Scalar[Float], op_r: (Vector[Float], Vector[Float], Scalar[Float]) => Scalar[Float]): Unit = {
    {
      val r1 = op(v1, v2)
      val r2 = new Scalar[Float]()
      val r3 = op_r(v1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
    {
      val r2 = new Scalar[Float]()
      val r3 = op_r(v1, v2, r2)
      val r1 = op(v1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
  }

  def testWithResultVS_V(v1: Vector[Float], s2: Float, op: (Vector[Float], Float) => Vector[Float], op_r: (Vector[Float], Float, Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(v1, s2)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(v1, s2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(v1, s2, r2)
      val r1 = op(v1, s2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVS_Vd(v1: Vector[Double], s2: Double, op: (Vector[Double], Double) => Vector[Double], op_r: (Vector[Double], Double, Vector[Double]) => Vector[Double]): Unit = {
    val length = {
      val r1 = op(v1, s2)
      val r2 = new Vector[Double](r1.length)
      val r3 = op_r(v1, s2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Double](length)
      val r3 = op_r(v1, s2, r2)
      val r1 = op(v1, s2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultVSS_V(v1: Vector[Float], s2: Int, s3: Int, op: (Vector[Float], Int, Int) => Vector[Float], op_r: (Vector[Float], Int, Int, Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(v1, s2, s3)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(v1, s2, s3, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(v1, s2, s3, r2)
      val r1 = op(v1, s2, s3)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Mbl[T: ClassTag](m1: Matrix[T], op: (Matrix[T]) => Matrix[Boolean], op_r: (Matrix[T], Matrix[Boolean]) => Matrix[Boolean]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Boolean](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Boolean](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Mi[T: ClassTag](m1: Matrix[T], op: (Matrix[T]) => Matrix[Int], op_r: (Matrix[T], Matrix[Int]) => Matrix[Int]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Int](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Int](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Mc[T: ClassTag](m1: Matrix[T], op: (Matrix[T]) => Matrix[Char], op_r: (Matrix[T], Matrix[Char]) => Matrix[Char]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Char](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Char](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Ml[T: ClassTag](m1: Matrix[T], op: (Matrix[T]) => Matrix[Long], op_r: (Matrix[T], Matrix[Long]) => Matrix[Long]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Long](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Long](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_M(m1: Matrix[Float], op: (Matrix[Float]) => Matrix[Float], op_r: (Matrix[Float], Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Md[T: ClassTag](m1: Matrix[T], op: (Matrix[T]) => Matrix[Double], op_r: (Matrix[T], Matrix[Double]) => Matrix[Double]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1)
      val r2 = new Matrix[Double](r1.rows, r1.columns)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Double](rows, columns)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_V(m1: Matrix[Float], op: Matrix[Float] => Vector[Float], op_r: (Matrix[Float], Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(m1)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_Vd(m1: Matrix[Double], op: Matrix[Double] => Vector[Double], op_r: (Matrix[Double], Vector[Double]) => Vector[Double]): Unit = {
    val length = {
      val r1 = op(m1)
      val r2 = new Vector[Double](r1.length)
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Double](length)
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultM_S(m1: Matrix[Float], op: Matrix[Float] => Scalar[Float], op_r: (Matrix[Float], Scalar[Float]) => Scalar[Float]): Unit = {
    {
      val r1 = op(m1)
      val r2 = new Scalar[Float]()
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
    {
      val r2 = new Scalar[Float]()
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
  }

  def testWithResultM_Sd(m1: Matrix[Double], op: Matrix[Double] => Scalar[Double], op_r: (Matrix[Double], Scalar[Double]) => Scalar[Double]): Unit = {
    {
      val r1 = op(m1)
      val r2 = new Scalar[Double]()
      val r3 = op_r(m1, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
    {
      val r2 = new Scalar[Double]()
      val r3 = op_r(m1, r2)
      val r1 = op(m1)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(r2.value() === r1.value())
    }
  }

  def testWithResultMM_M[V1: ClassTag, V2: ClassTag](m1: Matrix[V1], m2: Matrix[V2], op: (Matrix[V1], Matrix[V2]) => Matrix[Float], op_r: (Matrix[V1], Matrix[V2], Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1, m2)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(m1, m2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(m1, m2, r2)
      val r1 = op(m1, m2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultMM_Md[V1: ClassTag, V2: ClassTag](m1: Matrix[V1], m2: Matrix[V2], op: (Matrix[V1], Matrix[V2]) => Matrix[Double], op_r: (Matrix[V1], Matrix[V2], Matrix[Double]) => Matrix[Double]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1, m2)
      val r2 = new Matrix[Double](r1.rows, r1.columns)
      val r3 = op_r(m1, m2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Double](rows, columns)
      val r3 = op_r(m1, m2, r2)
      val r1 = op(m1, m2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultMV_M[M: ClassTag, V: ClassTag](m1: Matrix[M], v2: Vector[V], op: (Matrix[M], Vector[V]) => Matrix[Float], op_r: (Matrix[M], Vector[V], Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1, v2)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(m1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(m1, v2, r2)
      val r1 = op(m1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultMV_V[M: ClassTag, V: ClassTag, R: ClassTag](m1: Matrix[M], v2: Vector[V], op: (Matrix[M], Vector[V]) => Vector[R], op_r: (Matrix[M], Vector[V], Vector[R]) => Vector[R]): Unit = {
    val length = {
      val r1 = op(m1, v2)
      val r2 = new Vector[R](r1.length)
      val r3 = op_r(m1, v2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values().asInstanceOf[Array[Float]], r1.values().asInstanceOf[Array[Float]]) === 0)
      r1.length
    }
    {
      val r2 = new Vector[R](length)
      val r3 = op_r(m1, v2, r2)
      val r1 = op(m1, v2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values().asInstanceOf[Array[Float]], r1.values().asInstanceOf[Array[Float]]) === 0)
    }
  }

  def testWithResultMS_M(m1: Matrix[Float], s2: Float, op: (Matrix[Float], Float) => Matrix[Float], op_r: (Matrix[Float], Float, Matrix[Float]) => Matrix[Float]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1, s2)
      val r2 = new Matrix[Float](r1.rows, r1.columns)
      val r3 = op_r(m1, s2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Float](rows, columns)
      val r3 = op_r(m1, s2, r2)
      val r1 = op(m1, s2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultMS_Md(m1: Matrix[Double], s2: Double, op: (Matrix[Double], Double) => Matrix[Double], op_r: (Matrix[Double], Double, Matrix[Double]) => Matrix[Double]): Unit = {
    val (rows, columns) = {
      val r1 = op(m1, s2)
      val r2 = new Matrix[Double](r1.rows, r1.columns)
      val r3 = op_r(m1, s2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      (r1.rows, r1.columns)
    }
    {
      val r2 = new Matrix[Double](rows, columns)
      val r3 = op_r(m1, s2, r2)
      val r1 = op(m1, s2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def testWithResultMS_V(m1: Matrix[Float], s2: Int, op: (Matrix[Float], Int) => Vector[Float], op_r: (Matrix[Float], Int, Vector[Float]) => Vector[Float]): Unit = {
    val length = {
      val r1 = op(m1, s2)
      val r2 = new Vector[Float](r1.length)
      val r3 = op_r(m1, s2, r2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
      r1.length
    }
    {
      val r2 = new Vector[Float](length)
      val r3 = op_r(m1, s2, r2)
      val r1 = op(m1, s2)
      assert(r2 === r3)
      assert(r2 !== r1)
      assert(compare(r2.values(), r1.values()) === 0)
    }
  }

  def vectorbl(length: Int): Vector[Boolean] = {
    Vector.of(NativeVector.vectorData(length).map(_ >= 0))
  }

  def vectorc(length: Int): Vector[Char] = {
    Vector.of(NativeVector.vectorData(length).map(_.toChar))
  }

  def vectori(length: Int): Vector[Int] = {
    Vector.of(NativeVector.vectorData(length).map(_.toInt))
  }

  def vector(length: Int): Vector[Float] = {
    Vector.of(NativeVector.vectorData(length))
  }

  def vector(length: Int, generator: => Float): Vector[Float] = {
    Vector.of(NativeVector.vectorData(length, generator))
  }

  def vectord(length: Int): Vector[Double] = {
    Vector.of(NativeVector.vectorData(length).map(_.toDouble))
  }

  def vectord(length: Int, generator: => Float): Vector[Double] = {
    Vector.of(NativeVector.vectorData(length, generator).map(_.toDouble))
  }

  def matrix[T: ClassTag](rows: Int, columns: Int): Matrix[T] = {
    Matrix.of(NativeMatrix.matrixData(rows, columns))
  }

  def matrix[T: ClassTag](rows: Int, columns: Int, generator: => T): Matrix[T] = {
    Matrix.of(NativeMatrix.matrixData(rows, columns, generator))
  }

}
