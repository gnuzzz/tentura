package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Scalar._

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMatrix extends FunSuite with TestUtils {

  val ROWS = 513
//  val ROWS = 1024
//  val ROWS = 4
  val COLUMNS = 131
//  val COLUMNS = 1024
//  val COLUMNS = 2

  test("matrix + matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)

    val a = Matrix.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a + b
    result.copy2host()
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix + scalar") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 11.toByte

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 11.toShort

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 11

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 11l

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 11.2f

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 11.2

      val result = a + scalar
      result.copy2host()
      val nativeResult = nativeA + scalar.toFloat

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("matrix - matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a - b
    result.copy2host()
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix - scalar") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toByte

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toShort

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22l

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 22.1f

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.1

      val result = a - scalar
      result.copy2host()
      val nativeResult = nativeA - scalar.toFloat

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("scalar - matrix") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toByte

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toShort

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22l

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 22.1f

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar - nativeA

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.1

      val result = scalar - a
      result.copy2host()
      val nativeResult = scalar.toFloat - nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("matrix * matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeMatrix.matrix(COLUMNS, ROWS)
    val a = Matrix.of(nativeA.data)
    val b = Matrix.of(nativeB.data)
//    val a = Matrix[Float](Array(
//      Array(0.11424443f, -0.9391175f),
//      Array(-1.4517788f, -1.045195f)
//    ))
//    val b = Matrix[Float](Array(
//      Array(-0.8445823f, -1.25084f),
//      Array(-0.90989643f, -0.4892239f)
//    ))
    val result = a * b
    result.copy2host()
    val nativeResult = nativeA * nativeB

//    val error = errors(result.values(), nativeResult.data).filter(_._3 >= 0.01).head
//    print("Array(")
//    for (j <- nativeA.data(error._1).indices) {
//      print(nativeA.data(error._1)(j) + "f, ")
//    }
//    println(")")
//    print("Array(")
//    for (i <- nativeB.data.indices) {
//      print(nativeB.data(i)(error._2) + "f, ")
//    }
//    println(")")
//    println()
//    println(result.values()(error._1)(error._2))
//    println(nativeResult.data(error._1)(error._2))

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.1)
    println(s"$maxError")
//    printMatrix(aValues)
//    println()
//    printMatrix(bValues)
//    println()
//    printMatrix(resultValues)
//    println()
//    printMatrix(nativeResult.data)
  }

  test("matrix * vector") {
//    val nativeA = NativeMatrix(Array(Array(1.0f, 2.0f, 3.0f), Array(4.0f, 5.0f, 6.0f)))
//    val nativeB = NativeVector(Array(7.0f, 8.0f, 9.0f))
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeVector.vector(COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a * b
    result.copy2host()
    val nativeResult = nativeA * nativeB

//    printMatrix(nativeA.data)
//    printVector(nativeB.data)
//    printVector(result.values())
//    printVector(nativeResult.data)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.001)
    println(s"$maxError")
  }

  test("matrix * scalar") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 33.toByte

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 33.toShort

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 33

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 33l

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 33.4f

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 33.4

      val result = a * scalar
      result.copy2host()
      val nativeResult = nativeA * scalar.toFloat

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("matrix :* matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a :* b
    result.copy2host()
    val nativeResult = nativeA :* nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix / scalar") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 44.toByte

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 44.toShort

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 44

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 44l

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 44.1f

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 44.1

      val result = a / scalar
      result.copy2host()
      val nativeResult = nativeA / scalar.toFloat

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("scalar / matrix") {
    {
      val data = NativeMatrix.matrixData[Byte](ROWS, COLUMNS).map(_.map(Math.max(_, 1)))
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toByte

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toByte.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Short](ROWS, COLUMNS).map(_.map(Math.max(_, 1)))
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.toShort

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toShort.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Int](ROWS, COLUMNS).map(_.map(Math.max(_, 1)))
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toInt.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Long](ROWS, COLUMNS).map(_.map(Math.max(_, 1)))
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22l

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toLong.toFloat)))
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(Math.max(_, 0.1f)))
      val nativeA = NativeMatrix(data)
      val a = Matrix.of(data)
      val scalar = 22.1f

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar / nativeA

      val maxError = compare(result.values(), nativeResult.data)
      assert(maxError === 0.0f)
    }
    {
      val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS).map(_.map(Math.max(_, 0.1d)))
      val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
      val a = Matrix.of(data)
      val scalar = 22.1

      val result = scalar / a
      result.copy2host()
      val nativeResult = scalar.toFloat / nativeA

      val maxError = compare(result.values().map(_.map(_.toFloat)), nativeResult.data.map(_.map(_.toDouble.toFloat)))
      assert(maxError < 0.0001)
    }
  }

  test("matrix :/ matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Matrix.of(nativeB.data)

    val result = a :/ b
    result.copy2host()
    val nativeResult = nativeA :/ nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix pow") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

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

  test("matrix.t") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.t
    result.copy2host()
    val nativeResult = nativeA.t

    val maxError = compare(result.values(), nativeResult.data)

    assert(maxError === 0.0f) //todo
  }

  test("matrix.row") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.row(ROWS / 3)
    result.copy2host()
    val nativeResult = nativeA.row(ROWS / 3)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix.column") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.column(COLUMNS / 3)
    result.copy2host()
    val nativeResult = nativeA.column(COLUMNS / 3)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix sum") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.sum()
    result.copy2host()
    val nativeResult = nativeA.sum()

    assert(Math.abs(result.value() - nativeResult) / Math.abs(nativeResult) < 0.0001)
  }

  test("matrix sum rows") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.sum(axis = 0)
    result.copy2host()
    val nativeResult = nativeA.sum(axis = 0)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix sum columns") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = a.sum(axis = 1)
    result.copy2host()
    val nativeResult = nativeA.sum(axis = 1)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("matrix add row") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeVector.vector(COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a + b
    result.copy2host()
    val nativeResult = nativeA + nativeB

    val maxError = compare(result.values(), nativeResult.data)

    val data1 = result.values()
    val data2 = nativeResult.data
    for (i <- data1.indices) {
      val row1 = data1(i)
      val row2 = data2(i)
      for (j <- row2.indices) {
        val error = Math.abs(row1(j) - row2(j))
        assert(error < 0.0001, s"$i, $j, ${a.values()(i)(j)}, ${b.values()(j)}, ${result.values()(i)(j)}, ${nativeResult.data(i)(j)}")
      }
    }

    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("matrix add column") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Matrix.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a +| b
    result.copy2host()
    val nativeResult = nativeA +| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("matrix sub row") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeVector.vector(COLUMNS)
    val a = Matrix.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a - b
    result.copy2host()
    val nativeResult = nativeA - nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("matrix sub column") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val nativeB = NativeVector.vector(ROWS)
    val a = Matrix.of(nativeA.data)
    val b = Vector.of(nativeB.data)

    val result = a -| b
    result.copy2host()
    val nativeResult = nativeA -| nativeB

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
    println(s"$maxError")
  }
  
  test("matrix columns values") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val columnsIndicesData = Array.fill(ROWS)((Math.random() * COLUMNS).toInt)
    val nativeColumnIndices = new NativeVector(columnsIndicesData.map(_.toFloat))
    val a = Matrix.of(nativeA.data)
    val columnsIndices = Vector.of(columnsIndicesData)

    val result = a(columnsIndices)
    result.copy2host()
    val nativeResult = nativeA(nativeColumnIndices)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0)
  }

  /*
  test("aaa") {
    val a1 = Array(45.165096f, 27.024736f, 56.311897f, -153.86218f, -36.42162f, -89.54486f, -76.96f, -32.43854f, -73.05154f, -11.471472f,
    19.391636f, 33.638115f, 248.3375f, -187.46448f, -1.1224748f, 17.228935f, -25.159718f, 160.1502f, -16.250092f, 108.97274f, -57.002373f,
    75.00166f, 42.815147f, 143.84177f, 55.81227f, -149.43864f, 62.147964f, -28.181395f, 121.42378f, 5.5768228f, 64.57692f, 24.402954f,
    21.610558f, 3.887683f, 63.797134f, 64.24711f, 8.557238f, -54.11666f, 99.48951f, -80.50375f, -141.03473f, 28.57432f, 78.29953f,
    190.45361f, 17.895521f, -37.664356f, -67.293884f, 35.6486f, -16.69983f, -122.09855f, 359.89572f, 6.2843747f, 213.858f, -56.97526f,
    22.483492f, 10.8673725f, 1.9793037f, 185.1863f, 76.72348f, 141.7402f, -64.32123f, -4.0691805f, -10.018238f, 7.0248322f, 61.063694f,
    -12.215104f, 17.332754f, 163.29517f, 1.3487173f, -42.571545f, -13.192258f, 25.399433f, 273.38342f, 16.992939f, 27.40805f, 11.11546f,
    39.560184f, 110.48524f, -141.10434f, -77.67089f, -23.153051f, 187.90788f, 29.75257f, 10.179834f, 29.029099f, -17.31377f, -4.9603004f,
    -48.983242f, -74.19514f, 71.87494f, 64.608086f, 14.446436f, 60.16576f, -81.30003f, 137.85765f, -200.62047f, 45.09254f, -107.89272f,
    44.018894f, -34.845184f, -113.07317f, 62.83323f, -117.53483f, -75.57784f, 3.196787f, 45.095455f, -71.704666f, 34.754314f, 30.366398f,
    -231.08194f, -82.53392f, -62.508762f, 31.886179f, 40.787373f, -86.54605f, 77.677795f, 234.09392f, -62.084686f, 34.056187f, 45.447212f,
    -143.71906f, 12.839426f, -143.13858f, -93.01769f, -28.082233f, -50.98567f, 19.122276f, 156.13815f, 94.948494f, -227.22475f, 48.096436f)

    val a2 = Array(-48.472115f, -51.389168f, -214.94676f, -29.243631f, -100.58511f, -54.21528f, -47.56857f, -47.82181f, -56.275665f,
    123.37542f, 187.86728f, -9.587485f, -118.30605f, 64.761566f, -226.68753f, 23.578457f, 26.749155f, 68.87075f, 179.22679f, -109.80524f,
    -147.15337f, 46.066742f, 60.419445f, -59.3031f, 43.36048f, 45.718708f, 102.23434f, 98.11805f, -55.320892f, 52.407585f, 30.92286f,
    -32.60761f, 133.2775f, -35.095345f, -161.53856f, -106.01266f, -13.480817f, -7.647203f, -69.556885f, 100.751175f, -129.05583f, 81.12732f,
    20.486929f, 31.740208f, 141.21909f, 4.784598f, -72.95226f, -114.25122f, 33.690132f, -87.63661f, 16.212276f, 14.959797f, 92.053444f,
    -124.037025f, -53.265667f, -9.829049f, -25.801458f, -100.7788f, -183.47707f, -70.42346f, 166.88393f, -141.39696f, 114.70838f, 160.24219f,
    146.86978f, 85.713646f, -100.4692f, 171.03764f, -80.89707f, 41.73568f, 1.0480822f, -105.386826f, 125.09304f, -88.43234f, 25.456743f,
    -85.62405f, -39.716354f, 212.94559f, -123.52368f, 37.401417f, 45.624054f, 200.24748f, -15.311594f, 90.988815f, 109.116264f, -5.8495507f,
    -50.727955f, -148.6914f, 62.01241f, 53.917965f, 19.785051f, 301.8618f, -117.443855f, 111.72434f, -41.010345f, 153.4648f, -155.49803f,
    93.054184f, -54.85442f, 89.42562f, 151.55101f, -12.643359f, 224.76315f, 85.51832f, -107.48833f, 127.34915f, -5.1041136f, 113.07975f,
    92.77192f, -167.29256f, 108.374725f, -118.70781f, -8.642963f, 188.31165f, -8.968834f, -95.45681f, -73.903885f, -101.07126f, -127.841225f,
    49.2173f, -115.33979f, 22.297039f, 17.390799f, 49.88951f, -25.680115f, -107.893074f, -39.427204f, -4.9860063f, -33.225414f, 123.6331f, 10.00766f)
    //-0.42540878
    //-0.41333008
    val d1 = Vector.of(a1)
    val d2 = Vector.of(a2)
    val dr = d1 * d2
    dr.copy2host()
    println(dr.value())
//    dr.values().foreach(v => print(v + "\t"))
//    println()
//    val n1 = NativeVector(a1)
//    val n2 = NativeVector(a2)
//    val nr = n1 :* n2
//    nr.data.foreach(v => print(v + "\t"))
//    println()

//    val maxError = compare(dr.values(), nr.data)
//    assert(maxError === 0)

//    val ds = dr.sum()
//    ds.copy2host()
//    println(ds.value())
  }
  */

}
