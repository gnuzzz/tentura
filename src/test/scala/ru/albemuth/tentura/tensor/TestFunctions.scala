package ru.albemuth.tentura.tensor

import Functions.{sigmoid, exp}
import org.scalatest.FunSuite

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestFunctions extends FunSuite with TestUtils {

  val ROWS = 512
  val COLUMNS = 128

  test("scalar sigmoid") {
    val nativeScalar = 11.34f
    val scalar = new Scalar(nativeScalar)
    val result = sigmoid(scalar)
    val nativeResult = 1.0f / (1.0f + Math.exp(-nativeScalar))
    val maxError = Math.abs(result - nativeResult)
    assert(maxError < 0.0001)
  }

  test("vector sigmoid") {
    val nativeA = NativeVector.vector(ROWS)
    val a = new Vector(nativeA.data)

    val result = sigmoid(a)
    result.copy2host()
    val nativeResult = nativeA.sigmoid()

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
  }

  test("matrix sigmoid") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = sigmoid(a)
    result.copy2host()
    val nativeResult = nativeA.sigmoid()

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
  }

  test("vector exp") {
    val nativeA = NativeVector.vector(ROWS)
    val a = new Vector(nativeA.data)

    val result = exp(a)
    result.copy2host()
    val nativeResult = nativeA.exp()

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
  }

  test("matrix exp") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = exp(a)
    result.copy2host()
    val nativeResult = nativeA.exp()

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
  }
}
