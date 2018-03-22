package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Scalar._

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
    testWithResultVS_V(vector(COLUMNS), Math.random().toFloat, _ + _, _ + (_, _))
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
    testWithResultVS_V(vector(COLUMNS), Math.random().toFloat, _ - _, _ - (_, _))
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
    testWithResultVS_V(vector(COLUMNS), Math.random().toFloat, (v: Vector[Float], s: Float) => s - v, (v: Vector[Float], s: Float, r: Vector[Float]) => s - (v, r))
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
    testWithResultVS_V(vector(COLUMNS), Math.random().toFloat, _ * _, _ * (_, _))
  }

  test("vector * vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a * b
    val nativeResult = nativeA * nativeB
    val maxError = Math.abs(result - nativeResult) / Math.abs(nativeResult)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector * vector, result") {
    testWithResultVV_S(vector(COLUMNS), vector(COLUMNS), _ * _, _ * (_, _))
  }

  test("vector * matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a * b
    val nativeResult = nativeA * nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test ("vector * matrix, result") {
    testWithResultVM_V(vector(ROWS), matrix(ROWS, COLUMNS), _ * _, _ * (_, _))
  }

  test("vector |* vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a |* b
    val nativeResult = nativeA ** nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector |* vector, result") {
    testWithResultVV_M(vector(ROWS), vector(ROWS), _ |* _, _ |* (_, _))
  }

  test("vector :* vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a :* b
    val nativeResult = nativeA :* nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector :* vector, result") {
    testWithResultVV_V[Float, Float](vector(ROWS), vector(ROWS), _ :* _, _ :* (_, _))
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
    testWithResultVS_V(vector(ROWS), Math.random().toFloat, _ / _, _ / (_, _))
  }

  test("scalar / vector") {
    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toByte.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toShort

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toShort.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toInt.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11l

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toLong.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1))
      val nativeA = NativeVector(data)
      val a = Vector.of(data)
      val scalar = 11.2f

      val result = scalar / a
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0)
    }

    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toDouble)
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
    testWithResultVS_V(vector(ROWS), Math.random().toFloat, (v: Vector[Float], s: Float) => s / v, (v: Vector[Float], s: Float, r: Vector[Float]) => s / (v, r))
  }

  test("vector :/ vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a :/ b
    val nativeResult = nativeA :/ nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test ("vector :/ vector, result") {
    testWithResultVV_V[Float, Float](vector(ROWS), vector(ROWS), _ :/ _, _ :/ (_, _))
  }

  test("vector ^ pow") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      val nativeResult3 = nativeA ^ 3

//      val error = errors(Array(result3.values().map(_.toFloat)), Array(nativeResult3.data)).filter(_._1 >= 0.0001).head

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toShort)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val nativeA = NativeVector(data)
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
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
    testWithResultVM_M(vector(COLUMNS), matrix(ROWS, COLUMNS), _ + _, _ + (_, _))
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
    testWithResultVM_M(vector(ROWS), matrix(ROWS, COLUMNS), _ +| _, _ +| (_, _))
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
    testWithResultVM_M(vector(COLUMNS), matrix(ROWS, COLUMNS), _ - _, _ - (_, _))
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
    testWithResultVM_M(vector(ROWS), matrix(ROWS, COLUMNS), _ -| _, _ -| (_, _))
  }

  test("vector(i)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index = (Math.random() * ROWS).toInt
    val value = a(index)
    assert(value.value() === data(index))
  }

  test("vector(from, to)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index1 = Math.random() * ROWS
    val index2 = Math.random() * ROWS
    val from = Math.min(index1, index2).toInt
    val to = Math.max(index1, index2).toInt

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
    val value = Math.random().toFloat
    val index = (Math.random() * a.length).toInt
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
    val index1 = Math.random() * ROWS
    val index2 = Math.random() * ROWS
    val from = Math.min(index1, index2).toInt
    val to = Math.max(index1, index2).toInt

    def check(vector: Vector[Float], data: Array[Float]): Unit = {
      val maxError = compare(vector.values(), data)
      assert(maxError === 0)
    }

    check(a.slice(0, to), data.slice(0, to))
    check(a.slice(from, to), data.slice(from, to))

  }

  test("vector.slice(from, to, result)") {
    val index1 = Math.random() * ROWS
    val index2 = Math.random() * ROWS
    val from = Math.min(index1, index2).toInt
    val to = Math.max(index1, index2).toInt
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
        indices(i) = (Math.random() * ROWS).toInt
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

  test("vector.values(indices, result)") {
    def indices(length: Int): Array[Int] = {
      val indices = Array.ofDim[Int](length)
      for (i <- indices.indices) {
        indices(i) = (Math.random() * ROWS).toInt
      }
      indices
    }
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS)), _.values(_), _.values(_, _))
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS / 2)), _.values(_), _.values(_, _))
    testWithResultVV_V[Float, Int](vector(ROWS), Vector.of(indices(ROWS * 2)), _.values(_), _.values(_, _))
  }

}
