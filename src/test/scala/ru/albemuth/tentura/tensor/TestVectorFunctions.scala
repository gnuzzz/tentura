package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.VectorFunctions._
import ru.albemuth.tentura.tensor.kernel.vector._

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVectorFunctions extends FunSuite with TestUtils {

  val LENGTH = 513

  test("bincount(vector)") {
    val data = NativeVector.vectorData(10).map(_.toInt).map(v => if (v < 0) 0 else v)
    val nativeA = NativeVector(data.map(_.toFloat))
    val a = Vector.of(data)

    val result = bincount(a)
    result.copy2host()

    val nativeResult = NativeOperations.bincount(nativeA)

    val maxError = compare(result.values().map(_.toFloat), nativeResult.data)
    assert(maxError < 0.0001)

//    data.foreach(i => print(i + " "))
//    println()
//    result.values().zipWithIndex.foreach(p => println(p._2 + ": " + p._1))
  }

  test("sum(vector)") {
    def check(length: Int): Unit = {
      val data = NativeVector.vectorData(length)
      val a = Vector.of(data)

      val result = sum(a)
      result.copy2host()

      val nativeResult = data.sum
      val maxError = Math.abs(result - nativeResult)/Math.abs(nativeResult)
      assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
    }

    for (length <- 1 to 2 * VectorSum.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("max(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = max(a)
      result.copy2host()

      val nativeResult = nativeA.max()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorMax.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("min(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = min(a)
      result.copy2host()

      val nativeResult = nativeA.min()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorMin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmax(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmax(a)
      result.copy2host()

      val nativeResult = nativeA.argmax()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorArgmax.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmin(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmin(a)
      result.copy2host()

      val nativeResult = nativeA.argmin()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * VectorArgmin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

}
