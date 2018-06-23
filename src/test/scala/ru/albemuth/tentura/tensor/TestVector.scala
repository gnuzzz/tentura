package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Scalar._
import ru.albemuth.tentura.tensor.kernel.vector.Reverse

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVector extends FunSuite with TestUtils with TestWithResult {

  test("vector + vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a + b
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector + vector, result") {
    testWithResultVV_V[Float, Float](vector(COLUMNS), vector(COLUMNS), _ + _, _ + (_, _))
  }

  test ("vector += vector") {
    val data1 = NativeVector.vectorData(COLUMNS)
    val data2 = NativeVector.vectorData(COLUMNS)
    val v1 = Vector.of(data1)
    val v2 = Vector.of(data2)
    v1 += v2

    val maxError = compare(v1.values(), data1.zip(data2).map(v => v._1 + v._2))
    assert(maxError === 0.0f)
  }

  test("vector + scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a + scalar
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = a + scalar
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = a + scalar
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11l

      val result = a + scalar
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a + scalar
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a + scalar
      val nativeResult = nativeA + scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test ("vector + scalar, result") {
    testWithResultVS_V(vector(COLUMNS), java.lang.Math.random().toFloat, _ + _, _ + (_, _))
  }

  test ("vector += scalar") {
    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toByte)
      val scalar = 11.toByte
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => (v + scalar).toByte))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toShort)
      val scalar = 11.toShort
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => (v + scalar).toShort))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toInt)
      val scalar = 11
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => v + scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toLong)
      val scalar = 11.toLong
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => v + scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS)
      val scalar = 11.2f
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => v + scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toDouble)
      val scalar = 11.2
      val v1 = Vector.of(data1)
      v1 += scalar

      val maxError = compare(v1.values(), data1.map(v => v + scalar))
      assert(maxError === 0.0f)
    }
  }

  test("vector - vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a - b
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector - vector, result") {
    testWithResultVV_V[Float, Float](vector(COLUMNS), vector(COLUMNS), _ - _, _ - (_, _))
  }

  test ("vector -= vector") {
    val data1 = NativeVector.vectorData(COLUMNS)
    val data2 = NativeVector.vectorData(COLUMNS)
    val v1 = Vector.of(data1)
    val v2 = Vector.of(data2)
    v1 -= v2

    val maxError = compare(v1.values(), data1.zip(data2).map(v => v._1 - v._2))
    assert(maxError === 0.0f)
  }

  test("vector - scalar") {
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a - floatScalar
    val nativeResult = nativeA - floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector - scalar, result") {
    testWithResultVS_V(vector(COLUMNS), java.lang.Math.random().toFloat, _ - _, _ - (_, _))
  }

  test ("vector -= scalar") {
    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toByte)
      val scalar = 11.toByte
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => (v - scalar).toByte))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toShort)
      val scalar = 11.toShort
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => (v - scalar).toShort))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toInt)
      val scalar = 11
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => v - scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toLong)
      val scalar = 11.toLong
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => v - scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS)
      val scalar = 11.2f
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => v - scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toDouble)
      val scalar = 11.2
      val v1 = Vector.of(data1)
      v1 -= scalar

      val maxError = compare(v1.values(), data1.map(v => v - scalar))
      assert(maxError === 0.0f)
    }
  }

  test("scalar - vector") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = scalar - a
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = scalar - a
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = scalar - a
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS)
      val nativeA = NativeVector(data)
      val a = Vector.of(data)
      val scalar = 11.2f

      val result = scalar - a
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.2

      val result = scalar - a
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test ("scalar - vector, result") {
    testWithResultVS_V(vector(COLUMNS), java.lang.Math.random().toFloat, (v: Vector[Float], s: Float) => s - v, (v: Vector[Float], s: Float, r: Vector[Float]) => s - (v, r))
  }

  test("vector * scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a * scalar
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = a * scalar
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = a * scalar
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11l

      val result = a * scalar
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a * scalar
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a * scalar
      val nativeResult = nativeA * scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.001)
    }
  }

  test ("vector * scalar, result") {
    testWithResultVS_V(vector(COLUMNS), java.lang.Math.random().toFloat, _ * _, _ * (_, _))
  }

  test ("vector *= scalar") {
    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toByte)
      val scalar = 11.toByte
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => (v * scalar).toByte))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toShort)
      val scalar = 11.toShort
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => (v * scalar).toShort))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toInt)
      val scalar = 11
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => v * scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toLong)
      val scalar = 11.toLong
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => v * scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS)
      val scalar = 11.2f
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => v * scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toDouble)
      val scalar = 11.2
      val v1 = Vector.of(data1)
      v1 *= scalar

      val maxError = compare(v1.values(), data1.map(v => v * scalar))
      assert(maxError === 0.0f)
    }
  }

  test("vector *** vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a *** b
    val nativeResult = nativeA *** nativeB
    val maxError = java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector *** vector, result") {
    testWithResultVV_S(vector(COLUMNS), vector(COLUMNS), _ *** _, _ *** (_, _))
  }

  test("vector * matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a *** b
    val nativeResult = nativeA *** nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test ("vector * matrix, result") {
    testWithResultVM_V(vector(ROWS), matrix(ROWS, COLUMNS), _ *** _, _ *** (_, _))
  }

  test("vector |* vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a ***| b
    val nativeResult = nativeA ** nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector |* vector, result") {
    testWithResultVV_M(vector(ROWS), vector(ROWS), _ ***| _, _ ***| (_, _))
  }

  test("vector * vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a * b
    val nativeResult = nativeA * nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector * vector, result") {
    testWithResultVV_V[Float, Float](vector(ROWS), vector(ROWS), _ * _, _ * (_, _))
  }

  test ("vector *= vector") {
    val data1 = NativeVector.vectorData(COLUMNS)
    val data2 = NativeVector.vectorData(COLUMNS)
    val v1 = Vector.of(data1)
    val v2 = Vector.of(data2)
    v1 *= v2

    val maxError = compare(v1.values(), data1.zip(data2).map(v => v._1 * v._2))
    assert(maxError === 0.0f)
  }

  test("vector / scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a / scalar
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = a / scalar
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toShort.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = a / scalar
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toInt.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11l

      val result = a / scalar
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toLong.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a / scalar
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a / scalar
      val nativeResult = nativeA / scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test ("vector / scalar, result") {
    testWithResultVS_V(vector(ROWS), java.lang.Math.random().toFloat, _ / _, _ / (_, _))
  }

  test ("vector /= scalar") {
    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toByte)
      val scalar = 11.toByte
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => (v / scalar).toByte))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toShort)
      val scalar = 11.toShort
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => (v / scalar).toShort))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toInt)
      val scalar = 11
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => v / scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toLong)
      val scalar = 11.toLong
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => v / scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS)
      val scalar = 11.2f
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => v / scalar))
      assert(maxError === 0.0f)
    }

    {
      val data1 = NativeVector.vectorData(COLUMNS).map(_.toDouble)
      val scalar = 11.2
      val v1 = Vector.of(data1)
      v1 /= scalar

      val maxError = compare(v1.values(), data1.map(v => v / scalar))
      assert(maxError === 0.0f)
    }
  }

  test("scalar / vector") {
    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1).toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1).toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toShort.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1).toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toInt.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1).toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11l

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toLong.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1))
      val nativeA = NativeVector(data)
      val a = Vector.of(data)
      val scalar = 11.2f

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(java.lang.Math.max(_, 1).toDouble)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.2

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test ("scalar / vector, result") {
    testWithResultVS_V(vector(ROWS), java.lang.Math.random().toFloat, (v: Vector[Float], s: Float) => s / v, (v: Vector[Float], s: Float, r: Vector[Float]) => s / (v, r))
  }

  test("vector / vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a / b
    val nativeResult = nativeA / nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector / vector, result") {
    testWithResultVV_V[Float, Float](vector(ROWS), vector(ROWS), _ / _, _ / (_, _))
  }

  test ("vector /= vector") {
    val data1 = NativeVector.vectorData(COLUMNS)
    val data2 = NativeVector.vectorData(COLUMNS)
    val v1 = Vector.of(data1)
    val v2 = Vector.of(data2)
    v1 /= v2

    val maxError = compare(v1.values(), data1.zip(data2).map(v => v._1 / v._2))
    assert(maxError === 0.0f)
  }

  test("vector *^ pow") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

//      val error = errors(Array(result3.values().map(_.toFloat)), Array(nativeResult3.data)).filter(_._1 >= 0.0001).head

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val nativeA = NativeVector(data)
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a *^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a *^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a *^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
  }

  test("row + matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a + b
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("row + matrix, result") {
    testWithResultVM_M[Float, Float](vector(COLUMNS), matrix(ROWS, COLUMNS), _ + _, _ + (_, _))
  }

  test("column +| matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a +| b
    val nativeResult = nativeA +| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("column +| matrix, result") {
    testWithResultVM_M[Float, Float](vector(ROWS), matrix(ROWS, COLUMNS), _ +| _, _ +| (_, _))
  }

  test("row - matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a - b
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("row - matrix, result") {
    testWithResultVM_M[Float, Float](vector(COLUMNS), matrix(ROWS, COLUMNS), _ - _, _ - (_, _))
  }

  test("column -| matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a -| b
    val nativeResult = nativeA -| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("column -| matrix, result") {
    testWithResultVM_M[Float, Float](vector(ROWS), matrix(ROWS, COLUMNS), _ -| _, _ -| (_, _))
  }

  test("row * matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a * b
    val nativeResult = nativeA * nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("row * matrix, result") {
    testWithResultVM_M[Float, Float](vector(COLUMNS), matrix(ROWS, COLUMNS), _ * _, _ * (_, _))
  }

  test("column *| matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a *| b
    val nativeResult = nativeA *| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("column *| matrix, result") {
    testWithResultVM_M[Float, Float](vector(ROWS), matrix(ROWS, COLUMNS), _ *| _, _ *| (_, _))
  }

  test("row / matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a / b
    val nativeResult = nativeA / nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("row / matrix, result") {
    testWithResultVM_M[Float, Float](vector(COLUMNS), matrix(ROWS, COLUMNS), _ / _, _ / (_, _))
  }

  test("column /| matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a /| b
    val nativeResult = nativeA /| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("column /| matrix, result") {
    testWithResultVM_M[Float, Float](vector(ROWS), matrix(ROWS, COLUMNS), _ /| _, _ /| (_, _))
  }

  test("vector(i)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index = (java.lang.Math.random() * ROWS).toInt
    val value = a(index)
    assert(value.value() === data(index))
  }

  test("vector(from, to)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index1 = java.lang.Math.random() * ROWS
    val index2 = java.lang.Math.random() * ROWS
    val from = java.lang.Math.min(index1, index2).toInt
    val to = java.lang.Math.max(index1, index2).toInt

    def check(vector: Vector[Float], data: Array[Float]): Unit = {
      val maxError = compare(vector.values(), data)
      assert(maxError === 0)
    }

    check(a(0, to), data.slice(0, to))
    check(a(from, to), data.slice(from, to))

    val result = a(0, to)
    val value = a(0) + 1
    result(0) = value
    assert(a(0).value() === value)
  }

  test("vector(i) = value") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val value = java.lang.Math.random().toFloat
    val index = (java.lang.Math.random() * a.length).toInt
    a(index) = value
    assert(a(index).value() === value)
  }

  test("vector() = values") {
    val a = new Vector[Float](ROWS)
    val data = NativeVector.vectorData(ROWS)
    val values = Vector.of(data)
    a() = values
    val maxError = compare(a.values(), values.values())
    assert(maxError === 0)
  }

  test("vector(from, to) = values") {
    val a = new Vector[Float](ROWS)
    val data = NativeVector.vectorData(ROWS / 2)
    val values = Vector.of(data)
    a(a.length - values.length, a.length) = values
    val nativeResult = Array.ofDim[Float](ROWS)
    System.arraycopy(data, 0, nativeResult, a.length - values.length, data.length)
    val maxError = compare(a.values(), nativeResult)
    assert(maxError === 0)
  }

  test("vector.slice(from, to)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index1 = java.lang.Math.random() * ROWS
    val index2 = java.lang.Math.random() * ROWS
    val from = java.lang.Math.min(index1, index2).toInt
    val to = java.lang.Math.max(index1, index2).toInt

    def check(vector: Vector[Float], data: Array[Float]): Unit = {
      val maxError = compare(vector.values(), data)
      assert(maxError === 0)
    }

    check(a.slice(0, to), data.slice(0, to))
    check(a.slice(from, to), data.slice(from, to))

  }

  test("vector.slice(from, to, result)") {
    val index1 = java.lang.Math.random() * ROWS
    val index2 = java.lang.Math.random() * ROWS
    val from = java.lang.Math.min(index1, index2).toInt
    val to = java.lang.Math.max(index1, index2).toInt
    testWithResultVSS_V(vector(ROWS), from, to, _.slice(_, _), _.slice(_, _, _))
  }

  test("vector.values(indices)") {
    def values(data: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = Array.ofDim[Float](indices.length)
      for (i <- indices.indices) {
        result(i) = data(indices(i))
      }
      result
    }

    def indices(length: Int): Array[Int] = {
      val indices = Array.ofDim[Int](length)
      for (i <- indices.indices) {
        indices(i) = (java.lang.Math.random() * ROWS).toInt
      }
      indices
    }

    def check(data: Array[Float], indicesLength: Int): Unit = {
      val a = Vector.of(data)
      val ixs = indices(indicesLength)
      val result = a.values(Vector.of(ixs))
      assert(compare(result.values(), values(data, ixs)) === 0)
    }

    val data = NativeVector.vectorData(ROWS)
    check(data, ROWS)
    check(data, ROWS / 2)
    check(data, ROWS * 2)
  }

  test("vector.values(indices: Vector[Int], result)") {
    def indices(length: Int): Array[Int] = {
      val indices = Array.ofDim[Int](length)
      for (i <- indices.indices) {
        indices(i) = (java.lang.Math.random() * ROWS).toInt
      }
      indices
    }
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS)), _.values(_), _.values(_, _))
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS / 2)), _.values(_), _.values(_, _))
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS * 2)), _.values(_), _.values(_, _))
  }

  test("vector.values(indices: Matrix[Int], result)") {
    def indices(rows: Int, columns: Int): Array[Array[Int]] = {
      val indices = (for (i <- 0 to rows) yield {
        val row = Array.ofDim[Int](columns)
        for (i <- row.indices) {
          row(i) = (java.lang.Math.random() * COLUMNS).toInt
        }
        row
      }).toArray
      indices
    }

    {
      val data = NativeVector.vectorData(COLUMNS)
      val idxs = indices(ROWS, COLUMNS)
      val result = Vector.of(data).values(Matrix.of(idxs))

      val nativeResult = (for (i <- idxs.indices) yield {
        idxs(i).map(data(_))
      }).toArray

      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0)
    }

    testWithResultVM_M[Float, Int](vector(COLUMNS), Matrix.of(indices(ROWS, COLUMNS)), _.values(_), _.values(_, _))
    testWithResultVM_M[Float, Int](vector(COLUMNS), Matrix.of(indices(ROWS / 2 + 1, COLUMNS)), _.values(_), _.values(_, _))
    testWithResultVM_M[Float, Int](vector(COLUMNS), Matrix.of(indices(ROWS * 2 + 1, COLUMNS)), _.values(_), _.values(_, _))
  }

  test("vector.reverse()") {
    def test(length: Int): Unit = {
      val data = NativeVector.vectorData(length)
      val vector = Vector.of(data)
      val result = vector.reverse()

      val maxError = compare(result.values(), data.reverse)
      assert(maxError === 0)
    }

    test(4)
    test(5)
    test(COLUMNS)
    test(Reverse.TILE_DIM)
    test(Reverse.TILE_DIM + 1)
    test(Reverse.TILE_DIM + Reverse.TILE_DIM / 2)
    test(Reverse.TILE_DIM + Reverse.TILE_DIM)
    test(Reverse.TILE_DIM * 2 + Reverse.TILE_DIM / 2)
    test(Reverse.TILE_DIM * 4 + Reverse.TILE_DIM / 2 + 1)
  }

  test("vector.reverse(result)") {
    testWithResultV_Vf(vector(COLUMNS), _.reverse(), _.reverse(_))
  }

//  test("perf") {
//    println(JCudaKernel.numberOfSMs)
//
//    val data = NativeVector.vectorData(COLUMNS * 10000)
//    val vector = Vector.of(data)
//    val reversed = new Vector[Float](vector.length)
//
//    val t1 = System.currentTimeMillis()
//    for (i <- 0 to 10000) {
//      vector.reverse(reversed)
//    }
//    val t2 = System.currentTimeMillis()
//    println(s"reverse time: ${t2 - t1}")
//
//    val copy = new Vector[Float](vector.length)
//
//    val t3 = System.currentTimeMillis()
//    for (i <- 0 to 10000) {
//      copy.update(vector)
//    }
//    val t4 = System.currentTimeMillis()
//    println(s"copy time: ${t4 - t3}")
//  }

}
