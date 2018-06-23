package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.jcuda.jcusegsort.{Datatype, Sorting}
import ru.albemuth.tentura.tensor.SortOrder.SortOrder
import ru.albemuth.tentura.tensor.VectorFunctions._
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
    testWithResultV_Vi[Int](vector, VectorFunctions.bincount(_, maxValue), VectorFunctions.bincount(_, maxValue, _))
  }

  test("bincount loop") {
    val list = for (i <- 0 until 1000) yield {
      val closestY = Vector.of(Array(i))
      VectorFunctions.bincount(closestY)
    }
    assert(list.size === 1000)
  }

  test("multy bincount") {
    val vector = Vector.of(Array(0, 1, 2, 3, 4))
    val result = VectorFunctions.bincount(vector)
    assert(compare(result.values(), Array(1, 1, 1, 1, 1)) === 0)
    vector.copy2device(Array(0, 0, 0, 3, 4))
    VectorFunctions.bincount(vector)
    assert(compare(result.values(), Array(3, 0, 0, 1, 1)) === 0)
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

    for (length <- 1 to 2 * Sum.TILE_DIM) {
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

    for (length <- 1 to 2 * Max.TILE_DIM) {
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

    for (length <- 1 to 2 * Min.TILE_DIM) {
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

    for (length <- 1 to 2 * Argmax.TILE_DIM) {
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

    for (length <- 1 to 2 * Argmin.TILE_DIM) {
      check(length)
    }
    check(1000000)
  }

  test("argmin(vector, result)") {
    testWithResultV_S[Float, Int](vector(COLUMNS), VectorFunctions.argmin(_), VectorFunctions.argmin(_,  _))
  }

  test("sort(vector, context)") {
    def test[T: ClassTag](): Unit = {
      val data = vectorData[T](COLUMNS)
      val vector = Vector.of(data)
      val context = VectorFunctions.keySortContext(vector)
      val sortedVector = VectorFunctions.sort(vector, context)

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
      val sortedVector = VectorFunctions.sort(vector)

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
      val context = VectorFunctions.keySortContext(vector)
      val sortedVector = VectorFunctions.sort(vector, order, context)

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
      val sortedVector = VectorFunctions.sort(vector, order)

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
        val sortedIndices = VectorFunctions.sort(vector, context)
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
        val sortedIndices = VectorFunctions.argsort(vector, context)
      }
      Memory.print("device memory: ")
    }
  }
*/

  test("argsort(vector, context)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val context = Sorting.keyValueSortContext(Datatype.FLOAT, Datatype.INT, vector.length, 1)
    val sortedIndices = VectorFunctions.argsort(vector, context)

    val nativeVector = NativeVector(data)
    val nativeSortedIndices = NativeOperations.argsort(nativeVector)

    val maxError = compare(sortedIndices.values(), nativeSortedIndices.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("argsort(vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val sortedIndices = VectorFunctions.argsort(vector)

    val nativeVector = NativeVector(data)
    val nativeSortedIndices = NativeOperations.argsort(nativeVector)

    val maxError = compare(sortedIndices.values(), nativeSortedIndices.data.map(_.toInt))
    assert(maxError === 0)
  }

  //todo - tests for 2 argsort methods

}
