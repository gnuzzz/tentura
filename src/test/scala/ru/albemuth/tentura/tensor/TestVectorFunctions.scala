package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.jcuda.jcusegsort.{Datatype, Sorting}
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.Vector._
import ru.albemuth.tentura.tensor.kernel.vector._

import scala.reflect.ClassTag

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
    testWithResultV_Vi[Int](vector, Vector.bincount(_, maxValue), Vector.bincount(_, maxValue, _))
  }

  test("bincount loop") {
    val list = for (i <- 0 until 1000) yield {
      val closestY = Vector.of(Array(i))
      Vector.bincount(closestY)
    }
    assert(list.size === 1000)
  }

  test("multy bincount") {
    val vector = Vector.of(Array(0, 1, 2, 3, 4))
    val result = Vector.bincount(vector)
    assert(compare(result.values(), Array(1, 1, 1, 1, 1)) === 0)
    vector.copy2device(Array(0, 0, 0, 3, 4))
    Vector.bincount(vector)
    assert(compare(result.values(), Array(3, 0, 0, 1, 1)) === 0)
  }

  test("sum(vector)") {
    def check(length: Int): Unit = {
      val data = NativeVector.vectorData(length)
      val a = Vector.of(data)

      val result = sum(a)

      val nativeResult = data.sum
      val maxError = java.lang.Math.abs(result - nativeResult)/java.lang.Math.abs(nativeResult)
      assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
    }

    for (length <- 1 to 2 * Sum.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("sum(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), Vector.sum(_), Vector.sum(_,  _))
  }

  test("mean(vector)") {
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toInt)
        val a = Vector.of(data)

        val result = mean(a)

        val nativeResult = data.sum / data.length.toFloat
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length)
        val a = Vector.of(data)

        val result = mean(a)

        val nativeResult = data.sum / data.length.toFloat
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toDouble)
        val a = Vector.of(data)

        val result = mean(a)

        val nativeResult = data.sum.toFloat / data.length
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
  }

  test("mean(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), Vector.mean(_), Vector.mean(_,  _))
  }

  test("meand(vector)") {
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toInt)
        val a = Vector.of(data)

        val result = meand(a)

        val nativeResult = data.sum / data.length.toDouble
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length)
        val a = Vector.of(data)

        val result = meand(a)

        val nativeResult = data.map(_.toDouble).sum / data.length
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toDouble)
        val a = Vector.of(data)

        val result = meand(a)

        val nativeResult = data.sum / data.length.toDouble
        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
  }

  test("meand(vector, result)") {
    testWithResultV_S[Double, Double](vectord(COLUMNS), Vector.meand(_), Vector.meand(_,  _))
  }

  test("std(vector)") {
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toInt)
        val a = Vector.of(data)

        val result = std(a)

        val mx2 = data.map(v => v.toFloat * v.toFloat).sum / data.length.toFloat
        val mx = data.sum / data.length.toFloat
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx).toFloat

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length)
        val a = Vector.of(data)

        val result = std(a)

        val mx2 = data.map(v => v * v).sum / data.length.toFloat
        val mx = data.sum / data.length.toFloat
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx).toFloat

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toDouble)
        val a = Vector.of(data)

        val result = std(a)

        val mx2 = data.map(v => v * v).sum / data.length.toFloat
        val mx = data.sum / data.length.toFloat
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx).toFloat

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
  }

  test("std(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), Vector.std(_), Vector.std(_,  _))
  }

  test("stdd(vector)") {
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toInt)
        val a = Vector.of(data)

        val result = stdd(a)

        val mx2 = data.map(v => v.toFloat * v.toFloat).sum / data.length.toDouble
        val mx = data.sum / data.length.toDouble
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx)

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length)
//        val data = Array(66.630554f)
        val a = Vector.of(data)

        val result = stdd(a)

        val mx2 = data.map(v => v.toDouble * v.toDouble).sum / data.length
        val mx = data.map(_.toDouble).sum / data.length
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx)

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
    {
      def check(length: Int): Unit = {
        val data = NativeVector.vectorData(length).map(_.toDouble)
        val a = Vector.of(data)

        val result = stdd(a)

        val mx2 = data.map(v => v * v).sum / data.length
        val mx = data.sum / data.length
        val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx)

        val maxError = if (nativeResult == 0) java.lang.Math.abs(result - nativeResult) else java.lang.Math.abs(result - nativeResult) / java.lang.Math.abs(nativeResult)
        assert(maxError < 0.001, s"length: $length, expected: $nativeResult, actual: ${result.value()}")
      }

      for (length <- 1 to 2 * Sum.TILE_DIM) {
        check(length)
      }
      check(1000000)
    }
  }

  test("stdd(vector, result)") {
    testWithResultV_S[Double, Double](vectord(COLUMNS), Vector.stdd(_), Vector.stdd(_,  _))
  }

  test("max(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = max(a)

      val nativeResult = nativeA.max()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * Max.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("max(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), Vector.max(_), Vector.max(_,  _))
  }

  test("min(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = min(a)

      val nativeResult = nativeA.min()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * Min.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("min(vector, result)") {
    testWithResultV_S[Float, Float](vector(COLUMNS), Vector.min(_), Vector.min(_,  _))
  }

  test("argmax(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmax(a)

      val nativeResult = nativeA.argmax()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * Argmax.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmax(vector, result)") {
    testWithResultV_S[Float, Int](vector(COLUMNS), Vector.argmax(_), Vector.argmax(_,  _))
  }

  test("argmin(vector)") {
    def check(length: Int): Unit = {
      val nativeA = NativeVector.vector(length)
      val a = new Vector(nativeA.data)

      val result = argmin(a)

      val nativeResult = nativeA.argmin()
      assert(result.value() === nativeResult, s"length: $length")
    }

    for (length <- 1 to 2 * Argmin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmin(vector, result)") {
    testWithResultV_S[Float, Int](vector(COLUMNS), Vector.argmin(_), Vector.argmin(_,  _))
  }

  test("sort(vector, context)") {
    def test[T: ClassTag](): Unit = {
      val data = vectorData[T](COLUMNS)
      val vector = Vector.of(data)
      val context = Vector.keySortContext(vector)
      val sortedVector = Vector.sort(vector, context)

      val maxError = compare(sortedVector.values(), sorted(data))
      assert(maxError === 0)
    }
    test[Boolean]()
    test[Byte]()
    test[Char]()
    test[Short]()
    test[Int]()
    test[Long]()
    test[Float]()
    test[Double]()
  }

  test("sort(vector)") {
    def test[T: ClassTag](): Unit = {
      val data = vectorData[T](COLUMNS)
      val vector = Vector.of(data)
      val sortedVector = Vector.sort(vector)

      val maxError = compare(sortedVector.values(), sorted(data))
      assert(maxError === 0)
    }
    test[Boolean]()
    test[Byte]()
    test[Char]()
    test[Short]()
    test[Int]()
    test[Long]()
    test[Float]()
    test[Double]()
  }

  test("sort(vector, order, context)") {
    def test[T: ClassTag](order: SortOrder): Unit = {
      val data = vectorData[T](COLUMNS)
      val vector = Vector.of(data)
      val context = Vector.keySortContext(vector)
      val sortedVector = Vector.sort(vector, order, context)

      val maxError = compare(sortedVector.values(), if (order == SortOrder.ASC) sorted(data) else sorted(data).reverse)
      assert(maxError === 0)
    }
    test[Boolean](SortOrder.ASC)
    test[Byte](SortOrder.ASC)
    test[Char](SortOrder.ASC)
    test[Short](SortOrder.ASC)
    test[Int](SortOrder.ASC)
    test[Long](SortOrder.ASC)
    test[Float](SortOrder.ASC)
    test[Double](SortOrder.ASC)

    test[Boolean](SortOrder.DESC)
    test[Byte](SortOrder.DESC)
    test[Char](SortOrder.DESC)
    test[Short](SortOrder.DESC)
    test[Int](SortOrder.DESC)
    test[Long](SortOrder.DESC)
    test[Float](SortOrder.DESC)
    test[Double](SortOrder.DESC)
  }

  test("sort(vector, order)") {
    def test[T: ClassTag](order: SortOrder): Unit = {
      val data = vectorData[T](COLUMNS)
      val vector = Vector.of(data)
      val sortedVector = Vector.sort(vector, order)

      val maxError = compare(sortedVector.values(), if (order == SortOrder.ASC) sorted(data) else sorted(data).reverse)
      assert(maxError === 0)
    }
    test[Boolean](SortOrder.ASC)
    test[Byte](SortOrder.ASC)
    test[Char](SortOrder.ASC)
    test[Short](SortOrder.ASC)
    test[Int](SortOrder.ASC)
    test[Long](SortOrder.ASC)
    test[Float](SortOrder.ASC)
    test[Double](SortOrder.ASC)

    test[Boolean](SortOrder.DESC)
    test[Byte](SortOrder.DESC)
    test[Char](SortOrder.DESC)
    test[Short](SortOrder.DESC)
    test[Int](SortOrder.DESC)
    test[Long](SortOrder.DESC)
    test[Float](SortOrder.DESC)
    test[Double](SortOrder.DESC)
  }

  //todo - tests for 12 sort methods

/*
  test("loop sort(vector, context)") {
    val data = NativeVector.vectorData(2)
    val vector = Vector.of(data)
    val context = Sorting.keySortContext(Datatype.FLOAT, vector.length, 1)
    for (k <- 0 to 10) {
      for (i <- 0 to 1000) {
        val sortedIndices = Vector.sort(vector, context)
      }
      Memory.print("device memory: ")
    }
  }

  test("loop argsort(vector, context)") {
    val data = NativeVector.vectorData(4000)
    val vector = Vector.of(data)
    val context = Sorting.keyValueSortContext(Datatype.FLOAT, Datatype.INT, vector.length, 1)
    for (k <- 0 to 10) {
      for (i <- 0 to 1000) {
        val sortedIndices = Vector.argsort(vector, context)
      }
      Memory.print("device memory: ")
    }
  }
*/

  test("argsort(vector, context)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val context = Sorting.keyValueSortContext(Datatype.FLOAT, Datatype.INT, vector.length, 1)
    val sortedIndices = Vector.argsort(vector, context)

    val nativeVector = NativeVector(data)
    val nativeSortedIndices = NativeOperations.argsort(nativeVector)

    val maxError = compare(sortedIndices.values(), nativeSortedIndices.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("argsort(vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val sortedIndices = Vector.argsort(vector)

    val nativeVector = NativeVector(data)
    val nativeSortedIndices = NativeOperations.argsort(nativeVector)

    val maxError = compare(sortedIndices.values(), nativeSortedIndices.data.map(_.toInt))
    assert(maxError === 0)
  }

  //todo - tests for 2 argsort methods

}
