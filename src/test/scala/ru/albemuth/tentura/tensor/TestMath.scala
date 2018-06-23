package ru.albemuth.tentura.tensor

import Math._
import org.scalatest.FunSuite

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMath extends FunSuite with TestUtils with TestWithResult {

  test("abs(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = abs(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.abs))
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector = Vector.of(data)
      val result = abs(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.abs))
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_ >= 0)
      val vector = Vector.of(data)
      val result = abs(vector)
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toChar)
      val vector = Vector.of(data)
      val result = abs(vector)
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = abs(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.abs))
      assert(maxError === 0)
    }
  }

  test("abs(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), abs(_), abs(_, _))
    testWithResultV_Vi[Int](vectori(COLUMNS), abs(_), abs(_, _))
    testWithResultV_Vbl[Boolean](vectorbl(COLUMNS), abs(_), abs(_, _))
    testWithResultV_Vc(vectorc(COLUMNS), abs(_), abs(_, _))
    testWithResultV_Vd(vectord(COLUMNS), abs(_), abs(_, _))
  }
  
  test("abs(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.abs)))
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.abs)))
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_ >= 0))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toChar))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.abs)))
      assert(maxError === 0)
    }
  }

  test("abs(matrix, result)") {
    testWithResultM_M(matrix[Float](ROWS, COLUMNS), abs(_), abs(_, _))
    testWithResultM_Mi[Int](matrix[Int](ROWS, COLUMNS), abs(_), abs(_, _))
    testWithResultM_Mbl[Boolean](matrix[Boolean](ROWS, COLUMNS), abs(_), abs(_, _))
    testWithResultM_Mc[Char](matrix[Char](ROWS, COLUMNS), abs(_), abs(_, _))
    testWithResultM_Md[Double](matrix[Double](ROWS, COLUMNS), abs(_), abs(_, _))
  }
  
  test("acos(vector)") {
    {
      val data = NativeVector.vectorData(ROWS, java.lang.Math.random().toFloat)
      val vector = Vector.of(data)
      val result = acos(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.acos(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS, java.lang.Math.random().toFloat).map(_.toDouble)
      val vector = Vector.of(data)
      val result = acos(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.acos))
      assert(maxError < 0.0001)
    }
  }

  test("acos(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS, java.lang.Math.random().toFloat), acos(_), acos(_, _))
    testWithResultV_Vd(vectord(COLUMNS, java.lang.Math.random().toFloat), acos(_), acos(_, _))
  }
  
  test("acos(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS, java.lang.Math.random().toFloat)
      val matrix = Matrix.of(data)
      val result = acos(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.acos(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS, java.lang.Math.random())
      val matrix = Matrix.of(data)
      val result = acos(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.acos)))
      assert(maxError < 0.0001)
    }
  }

  test("acos(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS, java.lang.Math.random().toFloat), acos(_), acos(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS, java.lang.Math.random()), acos(_), acos(_, _))
  }

  test("acosh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = acosh(vector)
      val maxError = compare(result.values(), data.map(TestMath.acosh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = acosh(vector)
      val maxError = compare(result.values(), data.map(TestMath.acosh))
      assert(maxError < 0.0001)
    }
  }

  test("acosh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), acosh(_), acosh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), acosh(_), acosh(_, _))
  }

  test("acosh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = acosh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.acosh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = acosh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.acosh)))
      assert(maxError < 0.0001)
    }
  }

  test("acosh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), acosh(_), acosh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), acosh(_), acosh(_, _))
  }

  test("asin(vector)") {
    {
      val data = NativeVector.vectorData(ROWS, java.lang.Math.random().toFloat)
      val vector = Vector.of(data)
      val result = asin(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.asin(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS, java.lang.Math.random().toFloat).map(_.toDouble)
      val vector = Vector.of(data)
      val result = asin(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.asin))
      assert(maxError < 0.0001)
    }
  }

  test("asin(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS, java.lang.Math.random().toFloat), asin(_), asin(_, _))
    testWithResultV_Vd(vectord(COLUMNS, java.lang.Math.random().toFloat), asin(_), asin(_, _))
  }

  test("asin(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS, java.lang.Math.random().toFloat)
      val matrix = Matrix.of(data)
      val result = asin(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.asin(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS, java.lang.Math.random())
      val matrix = Matrix.of(data)
      val result = asin(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.asin)))
      assert(maxError < 0.0001)
    }
  }

  test("asin(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS, java.lang.Math.random().toFloat), asin(_), asin(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS, java.lang.Math.random()), asin(_), asin(_, _))
  }

  test("asinh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = asinh(vector)
      val maxError = compare(result.values(), data.map(TestMath.asinh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = asinh(vector)
      val maxError = compare(result.values(), data.map(TestMath.asinh))
      assert(maxError < 0.0001)
    }
  }

  test("asinh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), asinh(_), asinh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), asinh(_), asinh(_, _))
  }

  test("asinh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = asinh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.asinh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = asinh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.asinh)))
      assert(maxError < 0.0001)
    }
  }

  test("asinh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), asinh(_), asinh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), asinh(_), asinh(_, _))
  }

  test("atan(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = atan(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.atan(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = atan(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.atan))
      assert(maxError < 0.0001)
    }
  }

  test("atan(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), atan(_), atan(_, _))
    testWithResultV_Vd(vectord(COLUMNS), atan(_), atan(_, _))
  }

  test("atan(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = atan(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.atan(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = atan(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.atan)))
      assert(maxError < 0.0001)
    }
  }

  test("atan(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), atan(_), atan(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), atan(_), atan(_, _))
  }

  test("atanh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = atanh(vector)
      val maxError = compare(result.values(), data.map(TestMath.atanh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = atanh(vector)
      val maxError = compare(result.values(), data.map(TestMath.atanh))
      assert(maxError < 0.0001)
    }
  }

  test("atanh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), atanh(_), atanh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), atanh(_), atanh(_, _))
  }

  test("atanh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = atanh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.atanh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = atanh(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.atanh)))
      assert(maxError < 0.0001)
    }
  }

  test("atanh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), atanh(_), atanh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), atanh(_), atanh(_, _))
  }

  test("cbrt(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cbrt(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cbrt(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cbrt(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cbrt))
      assert(maxError < 0.0001)
    }
  }

  test("cbrt(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), cbrt(_), cbrt(_, _))
    testWithResultV_Vd(vectord(COLUMNS), cbrt(_), cbrt(_, _))
  }

  test("cbrt(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cbrt(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cbrt(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cbrt(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cbrt)))
      assert(maxError < 0.0001)
    }
  }

  test("cbrt(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), cbrt(_), cbrt(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), cbrt(_), cbrt(_, _))
  }

  test("ceil(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = ceil(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.ceil(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = ceil(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.ceil))
      assert(maxError < 0.0001)
    }
  }

  test("ceil(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), ceil(_), ceil(_, _))
    testWithResultV_Vd(vectord(COLUMNS), ceil(_), ceil(_, _))
  }

  test("ceil(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = ceil(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.ceil(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = ceil(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.ceil)))
      assert(maxError < 0.0001)
    }
  }

  test("ceil(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), ceil(_), ceil(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), ceil(_), ceil(_, _))
  }

  test("cos(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cos(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cos(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cos(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cos))
      assert(maxError < 0.0001)
    }
  }

  test("cos(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), cos(_), cos(_, _))
    testWithResultV_Vd(vectord(COLUMNS), cos(_), cos(_, _))
  }

  test("cos(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cos(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cos(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cos(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cos)))
      assert(maxError < 0.0001)
    }
  }

  test("cos(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), cos(_), cos(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), cos(_), cos(_, _))
  }

  test("cosh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cosh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cosh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cosh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.cosh))
      assert(maxError < 0.0001)
    }
  }

  test("cosh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), cosh(_), cosh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), cosh(_), cosh(_, _))
  }

  test("cosh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cosh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cosh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cosh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.cosh)))
      assert(maxError < 0.0001)
    }
  }

  test("cosh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), cosh(_), cosh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), cosh(_), cosh(_, _))
  }

  test("exp(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.exp(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.exp))
      assert(maxError < 0.0001)
    }
  }

  test("exp(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), exp(_), exp(_, _))
    testWithResultV_Vd(vectord(COLUMNS), exp(_), exp(_, _))
  }

  test("exp(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.exp(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.exp)))
      assert(maxError < 0.0001)
    }
  }

  test("exp(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), exp(_), exp(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), exp(_), exp(_, _))
  }

  test("exp10(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp10(vector)
      val maxError = compare(result.values(), data.map(TestMath.exp10(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp10(vector)
      val maxError = compare(result.values(), data.map(TestMath.exp10))
      assert(maxError < 0.0001)
    }
  }

  test("exp10(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), exp10(_), exp10(_, _))
    testWithResultV_Vd(vectord(COLUMNS), exp10(_), exp10(_, _))
  }

  test("exp10(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp10(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.exp10(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp10(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.exp10)))
      assert(maxError < 0.0001)
    }
  }

  test("exp10(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), exp10(_), exp10(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), exp10(_), exp10(_, _))
  }

  test("exp2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp2(vector)
      val maxError = compare(result.values(), data.map(TestMath.exp2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp2(vector)
      val maxError = compare(result.values(), data.map(TestMath.exp2))
      assert(maxError < 0.0001)
    }
  }

  test("exp2(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), exp2(_), exp2(_, _))
    testWithResultV_Vd(vectord(COLUMNS), exp2(_), exp2(_, _))
  }

  test("exp2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.exp2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.exp2)))
      assert(maxError < 0.0001)
    }
  }

  test("exp2(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), exp2(_), exp2(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), exp2(_), exp2(_, _))
  }

  test("floor(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = floor(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.floor(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = floor(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.floor))
      assert(maxError < 0.0001)
    }
  }

  test("floor(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), floor(_), floor(_, _))
    testWithResultV_Vd(vectord(COLUMNS), floor(_), floor(_, _))
  }

  test("floor(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = floor(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.floor(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = floor(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.floor)))
      assert(maxError < 0.0001)
    }
  }

  test("floor(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), floor(_), floor(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), floor(_), floor(_, _))
  }

  test("ln(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = ln(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.log(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = ln(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.log))
      assert(maxError < 0.0001)
    }
  }

  test("ln(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), ln(_), ln(_, _))
    testWithResultV_Vd(vectord(COLUMNS), ln(_), ln(_, _))
  }

  test("ln(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = ln(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.log(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = ln(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.log)))
      assert(maxError < 0.0001)
    }
  }

  test("ln(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), ln(_), ln(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), ln(_), ln(_, _))
  }

  test("log(vector, base)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log(vector, 3.0f)
      val maxError = compare(result.values(), data.map(TestMath.log(_, 3.0).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log(vector, 3.0)
      val maxError = compare(result.values(), data.map(TestMath.log(_, 3.0)))
      assert(maxError < 0.0001)
    }
  }

  test("log(vector, base, result)") {
    testWithResultV_Vf(vector(COLUMNS), log(_, 3.0f), log(_, 3.0f, _))
    testWithResultV_Vd(vectord(COLUMNS), log(_, 3.0), log(_, 3.0, _))
  }

  test("log(matrix, base)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log(matrix, 3.0f)
      val maxError = compare(result.values(), data.map(_.map(TestMath.log(_, 3.0f).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log(matrix, 3.0)
      val maxError = compare(result.values(), data.map(_.map(TestMath.log(_, 3.0))))
      assert(maxError < 0.0001)
    }
  }

  test("log(matrix, base, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), log(_, 3.0f), log(_, 3.0f, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), log(_, 3.0), log(_, 3.0, _))
  }

  test("log10(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log10(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.log10(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log10(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.log10))
      assert(maxError < 0.0001)
    }
  }

  test("log10(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), log10(_), log10(_, _))
    testWithResultV_Vd(vectord(COLUMNS), log10(_), log10(_, _))
  }

  test("log10(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log10(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.log10(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log10(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.log10)))
      assert(maxError < 0.0001)
    }
  }

  test("log10(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), log10(_), log10(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), log10(_), log10(_, _))
  }

  test("log2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log2(vector)
      val maxError = compare(result.values(), data.map(TestMath.log2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log2(vector)
      val maxError = compare(result.values(), data.map(TestMath.log2))
      assert(maxError < 0.0001)
    }
  }

  test("log2(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), log2(_), log2(_, _))
    testWithResultV_Vd(vectord(COLUMNS), log2(_), log2(_, _))
  }

  test("log2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.log2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.log2)))
      assert(maxError < 0.0001)
    }
  }

  test("log2(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), log2(_), log2(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), log2(_), log2(_, _))
  }

  test("max(vector, scalar)") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue().toInt
      val result = max(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.max(_, threshold)))
      assert(maxError == 0)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue()
      val result = max(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.max(_, threshold)))
      assert(maxError == 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue().toDouble
      val result = max(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.max(_, threshold)))
      assert(maxError == 0)
    }
  }

  test("max(vector, scalar, result)") {
    testWithResultVS_V(vector(COLUMNS), NativeVector.randomValue(), max(_, _), max(_, _, _))
    testWithResultVS_Vd(vectord(COLUMNS), NativeVector.randomValue().toDouble, max(_, _), max(_, _, _))
  }

  test("max(vector, vector)") {
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toInt)
      val data2 = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = max(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.max(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS)
      val data2 = NativeVector.vectorData(ROWS)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = max(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.max(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val data2 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = max(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.max(s._1, s._2)))
      assert(maxError == 0)
    }
  }

  test("max(vector, vector, result)") {
    testWithResultVV_V[Float, Float](vector(COLUMNS), vector(COLUMNS), max(_, _), max(_, _, _))
    testWithResultVV_Vd[Double, Double](vectord(COLUMNS), vectord(COLUMNS), max(_, _), max(_, _, _))
  }

  test("max(matrix, scalar)") {
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue().toInt
      val matrix = Matrix.of(data)
      val result = max(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Int](data, threshold, java.lang.Math.max(_: Int, _: Int)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue()
      val matrix = Matrix.of(data)
      val result = max(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Float](data, threshold, java.lang.Math.max(_: Float, _: Float)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue().toDouble
      val matrix = Matrix.of(data)
      val result = max(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Double](data, threshold, java.lang.Math.max(_: Double, _: Double)))
      assert(maxError < 0.0001)
    }
  }

  test("max(matrix, scalar, result)") {
    testWithResultMS_M(matrix(ROWS, COLUMNS), NativeVector.randomValue(), max(_, _), max(_, _, _))
    testWithResultMS_Md(matrix[Double](ROWS, COLUMNS), NativeVector.randomValue(), max(_, _), max(_, _, _))
  }

  test("max(matrix, matrix)") {
    {
      val data1 = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = max(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Int](data1, data2, java.lang.Math.max(_: Int, _: Int)))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = max(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Float](data1, data2, java.lang.Math.max(_: Float, _: Float)))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = max(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Double](data1, data2, java.lang.Math.max(_: Double, _: Double)))
      assert(maxError < 0.0001)
    }
  }

  test("max(matrix, matrix, result)") {
    testWithResultMM_M[Float, Float](matrix(ROWS, COLUMNS), matrix(ROWS, COLUMNS), max(_, _), max(_, _, _))
    testWithResultMM_Md[Double, Double](matrix[Double](ROWS, COLUMNS), matrix[Double](ROWS, COLUMNS), max(_, _), max(_, _, _))
  }

  test("min(vector, scalar)") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue().toInt
      val result = min(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.min(_, threshold)))
      assert(maxError == 0)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue()
      val result = min(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.min(_, threshold)))
      assert(maxError == 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val threshold = NativeVector.randomValue().toDouble
      val result = min(vector, threshold)
      val maxError = compare(result.values(), data.map(java.lang.Math.min(_, threshold)))
      assert(maxError == 0)
    }
  }

  test("min(vector, scalar, result)") {
    testWithResultVS_V(vector(COLUMNS), NativeVector.randomValue(), min(_, _), min(_, _, _))
    testWithResultVS_Vd(vectord(COLUMNS), NativeVector.randomValue().toDouble, min(_, _), min(_, _, _))
  }

  test("min(vector, vector)") {
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toInt)
      val data2 = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = min(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.min(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS)
      val data2 = NativeVector.vectorData(ROWS)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = min(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.min(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val data2 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = min(vector1, vector2)
      val maxError = compare(result.values(), data1.zip(data2).map(s => java.lang.Math.min(s._1, s._2)))
      assert(maxError == 0)
    }
  }

  test("min(vector, vector, result)") {
    testWithResultVV_V[Float, Float](vector(COLUMNS), vector(COLUMNS), min(_, _), min(_, _, _))
    testWithResultVV_Vd[Double, Double](vectord(COLUMNS), vectord(COLUMNS), min(_, _), min(_, _, _))
  }

  test("min(matrix, scalar)") {
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue().toInt
      val matrix = Matrix.of(data)
      val result = min(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Int](data, threshold, java.lang.Math.min(_: Int, _: Int)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue()
      val matrix = Matrix.of(data)
      val result = min(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Float](data, threshold, java.lang.Math.min(_: Float, _: Float)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val threshold = NativeVector.randomValue().toDouble
      val matrix = Matrix.of(data)
      val result = min(matrix, threshold)
      val maxError = compare(result.values(), TestMath.process[Double](data, threshold, java.lang.Math.min(_: Double, _: Double)))
      assert(maxError < 0.0001)
    }
  }

  test("min(matrix, scalar, result)") {
    testWithResultMS_M(matrix(ROWS, COLUMNS), NativeVector.randomValue(), min(_, _), min(_, _, _))
    testWithResultMS_Md(matrix[Double](ROWS, COLUMNS), NativeVector.randomValue(), min(_, _), min(_, _, _))
  }

  test("min(matrix, matrix)") {
    {
      val data1 = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = min(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Int](data1, data2, java.lang.Math.min(_: Int, _: Int)))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = min(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Float](data1, data2, java.lang.Math.min(_: Float, _: Float)))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = min(matrix1, matrix2)
      val maxError = compare(result.values(), TestMath.process[Double](data1, data2, java.lang.Math.min(_: Double, _: Double)))
      assert(maxError < 0.0001)
    }
  }

  test("min(matrix, matrix, result)") {
    testWithResultMM_M[Float, Float](matrix(ROWS, COLUMNS), matrix(ROWS, COLUMNS), min(_, _), min(_, _, _))
    testWithResultMM_Md[Double, Double](matrix[Double](ROWS, COLUMNS), matrix[Double](ROWS, COLUMNS), min(_, _), min(_, _, _))
  }

  test("pow(vector, power)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = pow(vector, 3.0f)
      val maxError = compare(result.values(), data.map(java.lang.Math.pow(_, 3.0).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = pow(vector, 3.0)
      val maxError = compare(result.values(), data.map(java.lang.Math.pow(_, 3.0)))
      assert(maxError < 0.0001)
    }
  }

  test("pow(vector, power, result)") {
    testWithResultV_Vf(vector(COLUMNS), pow(_, 3.0f), pow(_, 3.0f, _))
    testWithResultV_Vd(vectord(COLUMNS), pow(_, 3.0), pow(_, 3.0, _))
  }

  test("pow(matrix, power)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = pow(matrix, 3.0f)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.pow(_, 3.0f).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = pow(matrix, 3.0)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.pow(_, 3.0))))
      assert(maxError < 0.0001)
    }
  }

  test("pow(matrix, power, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), pow(_, 3.0f), pow(_, 3.0f, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), pow(_, 3.0), pow(_, 3.0, _))
  }

  test("pow2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = pow2(vector)
      val maxError = compare(result.values(), data.map(TestMath.pow2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = pow2(vector)
      val maxError = compare(result.values(), data.map(TestMath.pow2))
      assert(maxError < 0.0001)
    }
  }

  test("pow2(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), pow2(_), pow2(_, _))
    testWithResultV_Vd(vectord(COLUMNS), pow2(_), pow2(_, _))
  }

  test("pow2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = pow2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.pow2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = pow2(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.pow2)))
      assert(maxError < 0.0001)
    }
  }

  test("pow2(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), pow2(_), pow2(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), pow2(_), pow2(_, _))
  }

  test("relu(vector)") {
    {
      val data = NativeVector.vectorData(ROWS).map(_ > 0)
      val vector = Vector.of(data)
      val result = relu(vector)
      val maxError = compare(result.values(), data.map(TestMath.relu))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = relu(vector)
      val maxError = compare(result.values(), data.map(TestMath.relu(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = relu(vector)
      val maxError = compare(result.values(), data.map(TestMath.relu))
      assert(maxError < 0.0001)
    }
  }

  test("relu(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), relu(_), relu(_, _))
    testWithResultV_Vd(vectord(COLUMNS), relu(_), relu(_, _))
  }

  test("relu(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = relu(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.relu(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = relu(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.relu)))
      assert(maxError < 0.0001)
    }
  }

  test("relu(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), relu(_), relu(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), relu(_), relu(_, _))
  }

  test("round(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = round(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.round))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = roundd(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.round))
      assert(maxError < 0.0001)
    }
  }

  test("round(vector, result)") {
    testWithResultV_Vi[Float](vector(COLUMNS), round, round)
    testWithResultV_Vl[Double](vectord(COLUMNS), roundd(_), roundd(_, _))
  }

  test("round(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = round(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.round)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = roundd(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.round)))
      assert(maxError < 0.0001)
    }
  }

  test("round(matrix, result)") {
    testWithResultM_Mi[Float](matrix(ROWS, COLUMNS), round, round)
    testWithResultM_Ml[Double](matrix(ROWS, COLUMNS), roundd(_), roundd(_, _))
  }

  test("sigmoid(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sigmoid(vector)
      val maxError = compare(result.values(), data.map(TestMath.sigmoid(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sigmoid(vector)
      val maxError = compare(result.values(), data.map(TestMath.sigmoid))
      assert(maxError < 0.0001)
    }
  }

  test("sigmoid(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), sigmoid(_), sigmoid(_, _))
    testWithResultV_Vd(vectord(COLUMNS), sigmoid(_), sigmoid(_, _))
  }

  test("sigmoid(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sigmoid(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.sigmoid(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sigmoid(matrix)
      val maxError = compare(result.values(), data.map(_.map(TestMath.sigmoid)))
      assert(maxError < 0.0001)
    }
  }

  test("sigmoid(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), sigmoid(_), sigmoid(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), sigmoid(_), sigmoid(_, _))
  }

  test("sin(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sin(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sin(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sin(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sin))
      assert(maxError < 0.0001)
    }
  }

  test("sin(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), sin(_), sin(_, _))
    testWithResultV_Vd(vectord(COLUMNS), sin(_), sin(_, _))
  }

  test("sin(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sin(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sin(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sin(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sin)))
      assert(maxError < 0.0001)
    }
  }

  test("sin(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), sin(_), sin(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), sin(_), sin(_, _))
  }

  test("sinh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sinh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sinh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sinh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sinh))
      assert(maxError < 0.0001)
    }
  }

  test("sinh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), sinh(_), sinh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), sinh(_), sinh(_, _))
  }

  test("sinh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sinh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sinh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sinh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sinh)))
      assert(maxError < 0.0001)
    }
  }

  test("sinh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), sinh(_), sinh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), sinh(_), sinh(_, _))
  }

  test("sqrt(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sqrt(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sqrt(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sqrt(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.sqrt))
      assert(maxError < 0.0001)
    }
  }

  test("sqrt(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), sqrt(_), sqrt(_, _))
    testWithResultV_Vd(vectord(COLUMNS), sqrt(_), sqrt(_, _))
  }

  test("sqrt(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sqrt(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sqrt(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sqrt(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.sqrt)))
      assert(maxError < 0.0001)
    }
  }

  test("sqrt(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), sqrt(_), sqrt(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), sqrt(_), sqrt(_, _))
  }

  test("tan(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = tan(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.tan(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = tan(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.tan))
      assert(maxError < 0.0001)
    }
  }

  test("tan(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), tan(_), tan(_, _))
    testWithResultV_Vd(vectord(COLUMNS), tan(_), tan(_, _))
  }

  test("tan(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = tan(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.tan(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = tan(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.tan)))
      assert(maxError < 0.0001)
    }
  }

  test("tan(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), tan(_), tan(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), tan(_), tan(_, _))
  }

  test("tanh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = tanh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.tanh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = tanh(vector)
      val maxError = compare(result.values(), data.map(java.lang.Math.tanh))
      assert(maxError < 0.0001)
    }
  }

  test("tanh(vector, result)") {
    testWithResultV_Vf(vector(COLUMNS), tanh(_), tanh(_, _))
    testWithResultV_Vd(vectord(COLUMNS), tanh(_), tanh(_, _))
  }

  test("tanh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = tanh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.tanh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = tanh(matrix)
      val maxError = compare(result.values(), data.map(_.map(java.lang.Math.tanh)))
      assert(maxError < 0.0001)
    }
  }

  test("tanh(matrix, result)") {
    testWithResultM_M(matrix(ROWS, COLUMNS), tanh(_), tanh(_, _))
    testWithResultM_Md[Double](matrix(ROWS, COLUMNS), tanh(_), tanh(_, _))
  }
  
}

object TestMath {

  def acosh(x: Double): Double = java.lang.Math.log(x + java.lang.Math.sqrt(x * x - 1.0))

  def asinh(x: Double): Double = java.lang.Math.log(x + java.lang.Math.sqrt(x * x + 1.0))

  def atanh(x: Double): Double = 0.5 * java.lang.Math.log((x + 1.0) / (x - 1.0))

  def exp10(x: Double): Double = java.lang.Math.pow(10, x)

  def exp2(x: Double): Double = java.lang.Math.pow(2, x)

  def log(x: Double, base: Double): Double = java.lang.Math.log(x) / java.lang.Math.log(base)

  def log2(x: Double): Double = log(x, 2)

  def pow2(x: Double): Double = java.lang.Math.pow(x, 2)

  def relu(x: Boolean): Boolean = x

  def relu(x: Double): Double = if (x >= 0) x else 0

  def sigmoid(x: Double): Double = 1.0 / (1.0 + java.lang.Math.exp(-x))

  def process[T: ClassTag](m1: Array[Array[T]], s2: T, f: (T, T) => T): Array[Array[T]] = {
    val rows = for (i <- m1.indices) yield {
      val row1 = m1(i)
      row1.map(f(_, s2))
    }
    rows.toArray
  }

  def process[T: ClassTag](m1: Array[Array[T]], m2: Array[Array[T]], f: (T, T) => T): Array[Array[T]] = {
    val rows = for (i <- m1.indices) yield {
      val row1 = m1(i)
      val row2 = m2(i)
      row1.zip(row2).map(s => f(s._1, s._2))
    }
    rows.toArray
  }
}