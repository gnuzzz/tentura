package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.VectorFunctions._
import ru.albemuth.tentura.tensor.kernel.vector._

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVectorFunctions extends FunSuite with TestUtils with TestWithResult {

  test("bincount(vector)") {
    val data = NativeVector.vectorData(10).map(_.toInt).map(v => if (v < 0) 0 else v)
    val nativeA = NativeVector(data.map(_.toFloat))
    val a = Vector.of(data)

    val result = bincount(a)

    val nativeResult = NativeOperations.bincount(nativeA)

    val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
    assert(maxError < 0.0001)

  }

  test("bincount(vector, maxValue, result)") {
    val data = NativeVector.vectorData(COLUMNS).map(_.toInt).map(v => if (v < 0) 0 else v)
    val vector = Vector.of(data)
    val maxValue = vector.max().value()
    testWithResultV_Vi[Int](vector, VectorFunctions.bincount(_, maxValue), VectorFunctions.bincount(_, maxValue, _))
  }

  test("sum(vector)") {
    def check(length: Int): Unit = {
      val data = NativeVector.vectorData(length)
      val a = Vector.of(data)

      val result = sum(a)

      val nativeResult = data.sum
      val maxError = Math.abs(result - nativeResult)/Math.abs(nativeResult)
      assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
    }

    for (length <- 1 to 2 * VectorSum.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("sum(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), VectorFunctions.sum(_), VectorFunctions.sum(_,  _))
  }

  test("max(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = max(a)

      val nativeResult = nativeA.max()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorMax.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("max(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), VectorFunctions.max(_), VectorFunctions.max(_,  _))
  }

  test("min(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = min(a)

      val nativeResult = nativeA.min()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorMin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("min(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), VectorFunctions.min(_), VectorFunctions.min(_,  _))
  }

  test("argmax(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmax(a)

      val nativeResult = nativeA.argmax()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorArgmax.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmax(vector, result)") {
    testWithResultV_S[Float, Int](vector(COLUMNS), VectorFunctions.argmax(_), VectorFunctions.argmax(_,  _))
  }

  test("argmin(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmin(a)

      val nativeResult = nativeA.argmin()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorArgmin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmin(vector, result)") {
    testWithResultV_S[Float, Int](vector(COLUMNS), VectorFunctions.argmin(_), VectorFunctions.argmin(_,  _))
  }

}
