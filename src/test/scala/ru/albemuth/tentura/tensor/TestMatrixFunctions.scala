package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMatrixFunctions extends FunSuite with TestUtils with TestWithResult {

  test("MatrixFunctions.sum(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = MatrixFunctions.sum(a)
    val nativeResult = nativeA.sum()

    assert(Math.abs(result.value() - nativeResult) / Math.abs(nativeResult) < 0.0001)
  }

  test("MatrixFunctions.sum(matrix, result)") {
    testWithResultM_S(matrix(ROWS, COLUMNS), MatrixFunctions.sum(_), MatrixFunctions.sum(_, _))
  }

  test("MatrixFunctions.sumRows(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = MatrixFunctions.sum(a, axis = 0)
    val nativeResult = nativeA.sum(axis = 0)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("MatrixFunctions.sumRows(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), MatrixFunctions.sumRows(_), MatrixFunctions.sumRows(_, _))
  }

  test("MatrixFunctions.sumColumns(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = MatrixFunctions.sum(a, axis = 1)
    val nativeResult = nativeA.sum(axis = 1)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("MatrixFunctions.sumColumns(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), MatrixFunctions.sumColumns(_), MatrixFunctions.sumColumns(_, _))
  }

}
