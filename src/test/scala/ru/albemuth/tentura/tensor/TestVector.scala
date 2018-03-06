package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Scalar._

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVector extends FunSuite with TestUtils {

  val ROWS = 512
  val COLUMNS = 128

  test("vector + vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a + b
    result.copy2host()
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector + scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a + scalar
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test("vector - vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a - b
    result.copy2host()
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector - scalar") {
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a - floatScalar
    result.copy2host()
    val nativeResult = nativeA - floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("scalar - vector") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = scalar - a
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test("vector * scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a * scalar
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.001)
    }
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

  test("vector * matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a * b
    result.copy2host()
    val nativeResult = nativeA * nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector ** vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a ** b
    result.copy2host()
    val nativeResult = nativeA ** nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector :* vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a :* b
    result.copy2host()
    val nativeResult = nativeA :* nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector / scalar") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = a / scalar
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data.map(_.toLong.toFloat))
      assert(maxError < 0.0001)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data)
      val scalar = 11.2f

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }

    {
      val nativeA = NativeVector.vector(ROWS)
      val a = Vector.of(nativeA.data.map(_.toDouble))
      val scalar = 11.2

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar.toFloat

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test("scalar / vector") {
    {
      val data = NativeVector.vectorData(ROWS).map(Math.max(_, 1).toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)
      val scalar = 11.toByte

      val result = scalar / a
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
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
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
      assert(maxError < 0.0001)
    }
  }

  test("vector :/ vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a :/ b
    result.copy2host()
    val nativeResult = nativeA :/ nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector pow") {
    {
      val data = NativeVector.vectorData(ROWS).map(_.toByte)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
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
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toInt)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toLong)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS)
      val nativeA = NativeVector(data)
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
    {
      val data = NativeVector.vectorData(ROWS).map(_.toDouble)
      val nativeA = NativeVector(data.map(_.toFloat))
      val a = Vector.of(data)

      val result1 = a ^ 0.5f
      result1.copy2host()
      val nativeResult1 = nativeA ^ 0.5f

      val maxError1 = compare(result1.values(), nativeResult1.data)
      assert(maxError1 < 0.0001)

      val result2 = a ^ 2
      result2.copy2host()
      val nativeResult2 = nativeA ^ 2

      val maxError2 = compare(result2.values(), nativeResult2.data)
      assert(maxError2 < 0.0001)

      val result3 = a ^ 3
      result3.copy2host()
      val nativeResult3 = nativeA ^ 3

      val maxError3 = compare(result3.values(), nativeResult3.data)
      assert(maxError3 < 0.0001)
    }
  }

  test("vector row add matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a + b
    result.copy2host()
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector column add matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a +| b
    result.copy2host()
    val nativeResult = nativeA +| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector row sub matrix") {
    val nativeA = NativeVector.vector(COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a - b
    result.copy2host()
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector column sub matrix") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Vector.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a -| b
    result.copy2host()
    val nativeResult = nativeA -| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("vector(i)") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index = (Math.random() * ROWS).toInt
    val value = a(index)
    value.copy2host()
    assert(value.value() === data(index))
  }

  test("vector(from, to") {
    val data = NativeVector.vectorData(ROWS)
    val a = Vector.of(data)
    val index1 = Math.random() * ROWS
    val index2 = Math.random() * ROWS
    val from = Math.min(index1, index2).toInt
    val to = Math.max(index1, index2).toInt

    def check(vector: Vector[Float], data: Array[Float]): Unit = {
      vector.copy2host()
      val maxError = compare(vector.values(), data)
      assert(maxError === 0)
    }

    check(a.slice(0, to), data.slice(0, to))
    check(a.slice(from, to), data.slice(from, to))

  }

}
