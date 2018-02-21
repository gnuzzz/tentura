package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Scalar._

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVector extends FunSuite with TestUtils {

  val ROWS = 512
  val COLUMNS = 128

  //-Djava.library.path=D:/Vovan/lang/albemuth/tentura/lib

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
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a + floatScalar
    result.copy2host()
    val nativeResult = nativeA + floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = floatScalar - a
    result.copy2host()
    val nativeResult = floatScalar - nativeA

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector * scalar") {
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a * floatScalar
    result.copy2host()
    val nativeResult = nativeA * floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("vector * vector") {
    val nativeA = NativeVector.vector(ROWS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a * b
    val nativeResult = nativeA * nativeB
    val maxError = Math.abs(result - nativeResult)
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
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a / floatScalar
    result.copy2host()
    val nativeResult = nativeA / floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("scalar / vector") {
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)
    val floatScalar = 11.2f

    val result = floatScalar / a
    result.copy2host()
    val nativeResult = floatScalar / nativeA

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)

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

  test("vector sum") {
    val nativeA = NativeVector.vector(ROWS)
    val a = Vector.of(nativeA.data)

    val result = a.sum()
    result.copy2host()

    val nativeResult = nativeA.sum()

    assert(result.value() === nativeResult)
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

}
