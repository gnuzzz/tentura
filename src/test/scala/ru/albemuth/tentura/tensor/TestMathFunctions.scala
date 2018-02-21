package ru.albemuth.tentura.tensor

import MathFunctions._
import TestMathFunctions.{COLUMNS, ROWS}
import org.scalatest.FunSuite

import scala.reflect.ClassTag

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMathFunctions extends FunSuite with TestUtils {

  test("abs(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = abs(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.abs(_)))
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val vector = Vector.of(data)
      val result = abs(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.abs(_)))
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_ >= 0)
      val vector = Vector.of(data)
      val result = abs(vector)
      result.copy2host()
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toChar)
      val vector = Vector.of(data)
      val result = abs(vector)
      result.copy2host()
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = abs(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.abs(_)))
      assert(maxError === 0)
    }
  }
  
  test("abs(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.abs(_))))
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.abs(_))))
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_ >= 0))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toChar))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data)
      assert(maxError === 0)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = abs(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.abs(_))))
      assert(maxError === 0)
    }
  }
  
  test("acos(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = acos(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.acos(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = acos(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.acos(_)))
      assert(maxError < 0.0001)
    }
  }
  
  test("acos(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = acos(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.acos(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = acos(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.acos(_))))
      assert(maxError < 0.0001)
    }
  }

  test("acosh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = acosh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.acosh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = acosh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.acosh))
      assert(maxError < 0.0001)
    }
  }

  test("acosh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = acosh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.acosh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = acosh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.acosh)))
      assert(maxError < 0.0001)
    }
  }

  test("asin(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = asin(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.asin(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = asin(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.asin(_)))
      assert(maxError < 0.0001)
    }
  }

  test("asin(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = asin(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.asin(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = asin(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.asin(_))))
      assert(maxError < 0.0001)
    }
  }

  test("asinh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = asinh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.asinh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = asinh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.asinh))
      assert(maxError < 0.0001)
    }
  }

  test("asinh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = asinh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.asinh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = asinh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.asinh)))
      assert(maxError < 0.0001)
    }
  }

  test("atan(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = atan(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.atan(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = atan(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.atan(_)))
      assert(maxError < 0.0001)
    }
  }

  test("atan(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = atan(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.atan(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = atan(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.atan(_))))
      assert(maxError < 0.0001)
    }
  }

  test("atanh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = atanh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.atanh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = atanh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.atanh))
      assert(maxError < 0.0001)
    }
  }

  test("atanh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = atanh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.atanh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = atanh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.atanh)))
      assert(maxError < 0.0001)
    }
  }

  test("cbrt(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cbrt(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cbrt(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cbrt(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cbrt(_)))
      assert(maxError < 0.0001)
    }
  }

  test("cbrt(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cbrt(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cbrt(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cbrt(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cbrt(_))))
      assert(maxError < 0.0001)
    }
  }

  test("ceil(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = ceil(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.ceil(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = ceil(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.ceil(_)))
      assert(maxError < 0.0001)
    }
  }

  test("ceil(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = ceil(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.ceil(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = ceil(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.ceil(_))))
      assert(maxError < 0.0001)
    }
  }

  test("cos(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cos(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cos(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cos(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cos(_)))
      assert(maxError < 0.0001)
    }
  }

  test("cos(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cos(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cos(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cos(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cos(_))))
      assert(maxError < 0.0001)
    }
  }

  test("cosh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = cosh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cosh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = cosh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.cosh(_)))
      assert(maxError < 0.0001)
    }
  }

  test("cosh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = cosh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cosh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = cosh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.cosh(_))))
      assert(maxError < 0.0001)
    }
  }

  test("exp(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.exp(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.exp(_)))
      assert(maxError < 0.0001)
    }
  }

  test("exp(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.exp(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.exp(_))))
      assert(maxError < 0.0001)
    }
  }

  test("exp10(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp10(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.exp10(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp10(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.exp10))
      assert(maxError < 0.0001)
    }
  }

  test("exp10(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp10(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.exp10(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp10(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.exp10)))
      assert(maxError < 0.0001)
    }
  }

  test("exp2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = exp2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.exp2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = exp2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.exp2))
      assert(maxError < 0.0001)
    }
  }

  test("exp2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = exp2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.exp2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = exp2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.exp2)))
      assert(maxError < 0.0001)
    }
  }

  test("floor(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = floor(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.floor(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = floor(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.floor(_)))
      assert(maxError < 0.0001)
    }
  }

  test("floor(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = floor(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.floor(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = floor(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.floor(_))))
      assert(maxError < 0.0001)
    }
  }

  test("ln(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = ln(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.log(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = ln(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.log(_)))
      assert(maxError < 0.0001)
    }
  }

  test("ln(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = ln(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.log(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = ln(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.log(_))))
      assert(maxError < 0.0001)
    }
  }

  test("log(vector, base)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log(vector, 3.0f)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.log(_, 3.0).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log(vector, 3.0)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.log(_, 3.0)))
      assert(maxError < 0.0001)
    }
  }

  test("log(matrix, base)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log(matrix, 3.0f)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.log(_, 3.0f).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log(matrix, 3.0)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.log(_, 3.0))))
      assert(maxError < 0.0001)
    }
  }

  test("log10(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log10(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.log10(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log10(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.log10(_)))
      assert(maxError < 0.0001)
    }
  }

  test("log10(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log10(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.log10(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log10(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.log10(_))))
      assert(maxError < 0.0001)
    }
  }

  test("log2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = log2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.log2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = log2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.log2))
      assert(maxError < 0.0001)
    }
  }

  test("log2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = log2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.log2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = log2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.log2)))
      assert(maxError < 0.0001)
    }
  }

  test("max(vector, vector)") {
    {
      val data1 = NativeVector.vectorData(ROWS)
      val data2 = NativeVector.vectorData(ROWS)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = max(vector1, vector2)
      result.copy2host()
      val maxError = compare(result.values(), data1.zip(data2).map(s => Math.max(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val data2 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = max(vector1, vector2)
      result.copy2host()
      val maxError = compare(result.values(), data1.zip(data2).map(s => Math.max(s._1, s._2)))
      assert(maxError == 0)
    }
  }

  test("max(matrix, matrix)") {
    {
      val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = max(matrix1, matrix2)
      result.copy2host()
      val maxError = compare(result.values(), TestMathFunctions.process[Float](data1, data2, Math.max))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = max(matrix1, matrix2)
      result.copy2host()
      val maxError = compare(result.values(), TestMathFunctions.process[Double](data1, data2, Math.max))
      assert(maxError < 0.0001)
    }
  }

  test("min(vector, vector)") {
    {
      val data1 = NativeVector.vectorData(ROWS)
      val data2 = NativeVector.vectorData(ROWS)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = min(vector1, vector2)
      result.copy2host()
      val maxError = compare(result.values(), data1.zip(data2).map(s => Math.min(s._1, s._2)))
      assert(maxError == 0)
    }
    {
      val data1 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val data2 = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector1 = Vector.of(data1)
      val vector2 = Vector.of(data2)
      val result = min(vector1, vector2)
      result.copy2host()
      val maxError = compare(result.values(), data1.zip(data2).map(s => Math.min(s._1, s._2)))
      assert(maxError == 0)
    }
  }

  test("min(matrix, matrix)") {
    {
      val data1 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = min(matrix1, matrix2)
      result.copy2host()
      val maxError = compare(result.values(), TestMathFunctions.process[Float](data1, data2, Math.min))
      assert(maxError < 0.0001)
    }
    {
      val data1 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val data2 = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val matrix1 = Matrix.of(data1)
      val matrix2 = Matrix.of(data2)
      val result = min(matrix1, matrix2)
      result.copy2host()
      val maxError = compare(result.values(), TestMathFunctions.process[Double](data1, data2, Math.min))
      assert(maxError < 0.0001)
    }
  }

  test("pow(vector, power)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = pow(vector, 3.0f)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.pow(_, 3.0).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = pow(vector, 3.0)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.pow(_, 3.0)))
      assert(maxError < 0.0001)
    }
  }

  test("pow(matrix, power)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = pow(matrix, 3.0f)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.pow(_, 3.0f).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = pow(matrix, 3.0)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.pow(_, 3.0))))
      assert(maxError < 0.0001)
    }
  }

  test("pow2(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = pow2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.pow2(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = pow2(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.pow2))
      assert(maxError < 0.0001)
    }
  }

  test("pow2(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = pow2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.pow2(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = pow2(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.pow2)))
      assert(maxError < 0.0001)
    }
  }

  test("relu(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = relu(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.relu(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = relu(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.relu))
      assert(maxError < 0.0001)
    }
  }

  test("relu(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = relu(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.relu(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = relu(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.relu)))
      assert(maxError < 0.0001)
    }
  }

  test("round(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = round(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.round(_)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = roundd(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.round(_)))
      assert(maxError < 0.0001)
    }
  }

  test("round(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = round(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.round(_))))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = roundd(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.round(_))))
      assert(maxError < 0.0001)
    }
  }

  test("sigmoid(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sigmoid(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.sigmoid(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sigmoid(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(TestMathFunctions.sigmoid))
      assert(maxError < 0.0001)
    }
  }

  test("sigmoid(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sigmoid(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.sigmoid(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sigmoidd(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(TestMathFunctions.sigmoid)))
      assert(maxError < 0.0001)
    }
  }

  test("sin(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sin(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sin(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sin(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sin(_)))
      assert(maxError < 0.0001)
    }
  }

  test("sin(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sin(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sin(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sin(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sin(_))))
      assert(maxError < 0.0001)
    }
  }

  test("sinh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sinh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sinh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sinh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sinh(_)))
      assert(maxError < 0.0001)
    }
  }

  test("sinh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sinh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sinh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sinh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sinh(_))))
      assert(maxError < 0.0001)
    }
  }

  test("sqrt(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = sqrt(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sqrt(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = sqrt(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.sqrt(_)))
      assert(maxError < 0.0001)
    }
  }

  test("sqrt(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = sqrt(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sqrt(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = sqrt(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.sqrt(_))))
      assert(maxError < 0.0001)
    }
  }

  test("tan(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = tan(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.tan(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = tan(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.tan(_)))
      assert(maxError < 0.0001)
    }
  }

  test("tan(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = tan(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.tan(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = tan(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.tan(_))))
      assert(maxError < 0.0001)
    }
  }

  test("tanh(vector)") {
    {
      val data = NativeVector.vectorData(ROWS)
      val vector = Vector.of(data)
      val result = tanh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.tanh(_).toFloat))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val vector = Vector.of(data)
      val result = tanh(vector)
      result.copy2host()
      val maxError = compare(result.values(), data.map(Math.tanh(_)))
      assert(maxError < 0.0001)
    }
  }

  test("tanh(matrix)") {
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val matrix = Matrix.of(data)
      val result = tanh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.tanh(_).toFloat)))
      assert(maxError < 0.0001)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(_.toDouble))
      val matrix = Matrix.of(data)
      val result = tanh(matrix)
      result.copy2host()
      val maxError = compare(result.values(), data.map(_.map(Math.tanh(_))))
      assert(maxError < 0.0001)
    }
  }
  
}

object TestMathFunctions {
  val ROWS = 512
  val COLUMNS = 128

  def acosh(x: Double): Double = Math.log(x + Math.sqrt(x * x - 1.0))

  def asinh(x: Double): Double = Math.log(x + Math.sqrt(x * x + 1.0))

  def atanh(x: Double): Double = 0.5 * Math.log((x + 1.0) / (x - 1.0))

  def exp10(x: Double): Double = Math.pow(10, x)

  def exp2(x: Double): Double = Math.pow(2, x)

  def log(x: Double, base: Double): Double = Math.log(x) / Math.log(base)

  def log2(x: Double): Double = log(x, 2)

  def pow2(x: Double): Double = Math.pow(x, 2)

  def relu(x: Double): Double = if (x >= 0) x else 0

  def sigmoid(x: Double): Double = 1.0 / (1.0 + Math.exp(-x))

  def process[T: ClassTag](m1: Array[Array[T]], m2: Array[Array[T]], f: (T, T) => T): Array[Array[T]] = {
    val rows = for (i <- m1.indices) yield {
      val row1 = m1(i)
      val row2 = m2(i)
      row1.zip(row2).map(s => f(s._1, s._2))
    }
    rows.toArray
  }
}