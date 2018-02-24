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

  //-Djava.library.path=D:/Vovan/lang/albemuth/tentura/lib

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
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 11.2f

    val result = a + floatScalar
    result.copy2host()
    val nativeResult = nativeA + floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 22.1f

    val result = a - floatScalar
    result.copy2host()
    val nativeResult = nativeA - floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("scalar - matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 22.1f

    val result = floatScalar - a
    result.copy2host()
    val nativeResult = floatScalar - nativeA

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001)
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
    assert(maxError < 0.0001)
    println(s"$maxError")
  }

  test("matrix * scalar") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 33.4f

    val result = a * floatScalar
    result.copy2host()
    val nativeResult = nativeA * floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 44.1f

    val result = a / floatScalar
    result.copy2host()
    val nativeResult = nativeA / floatScalar

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("scalar / matrix") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)
    val floatScalar = 22.1f

    val result = floatScalar / a
    result.copy2host()
    val nativeResult = floatScalar / nativeA

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
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

    assert(Math.abs(result.value() - nativeResult) < 0.001)
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
    assert(maxError < 0.0001)
  }

}
