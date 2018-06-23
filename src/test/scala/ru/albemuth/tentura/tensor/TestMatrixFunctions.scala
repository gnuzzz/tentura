package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.jcuda.jcusegsort.{Datatype, Sorting}

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

  test("MatrixFunctions.min(matrix, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Scalar[Float]()
    val min = MatrixFunctions.min(matrix, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min()

    assert(min.value() === nativeMin)
  }

  test("MatrixFunctions.min(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = MatrixFunctions.min(matrix)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min()

    assert(min.value() === nativeMin)
  }

  test("MatrixFunctions.min(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](COLUMNS)
    val min = MatrixFunctions.min(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 0)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.min(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](ROWS)
    val min = MatrixFunctions.min(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 1)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.min(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = MatrixFunctions.min(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 0)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.min(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = MatrixFunctions.min(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 1)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.max(matrix, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Scalar[Float]()
    val max = MatrixFunctions.max(matrix, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max()

    assert(max.value() === nativeMax)
  }

  test("MatrixFunctions.max(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val max = MatrixFunctions.max(matrix)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max()

    assert(max.value() === nativeMax)
  }

  test("MatrixFunctions.max(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](COLUMNS)
    val max = MatrixFunctions.max(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max(axis = 0)

    val maxError = compare(max.values, nativeMax.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.max(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](ROWS)
    val max = MatrixFunctions.max(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max(axis = 1)

    val maxError = compare(max.values, nativeMax.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmin(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](COLUMNS)
    val argmin = MatrixFunctions.argmin(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 0)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmin(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](ROWS)
    val argmin = MatrixFunctions.argmin(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 1)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmin(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmin = MatrixFunctions.argmin(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 0)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmin(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmin = MatrixFunctions.argmin(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 1)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmax(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](COLUMNS)
    val argmax = MatrixFunctions.argmax(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 0)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmax(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](ROWS)
    val argmax = MatrixFunctions.argmax(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 1)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmax(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmax = MatrixFunctions.argmax(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 0)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argmax(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmax = MatrixFunctions.argmax(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 1)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("MatrixFunctions.sort(matrix, context)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val context = Sorting.keySortContext(Datatype.FLOAT, matrix.rows * matrix.columns, matrix.rows)
    val sortedMatrix = MatrixFunctions.sort(matrix, context)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeMatrix = NativeOperations.sort(nativeMatrix)

    val maxError = compare(sortedMatrix.values(), sortedNativeMatrix.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.sort(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val sortedMatrix = MatrixFunctions.sort(matrix)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeMatrix = NativeOperations.sort(nativeMatrix)

    val maxError = compare(sortedMatrix.values(), sortedNativeMatrix.data)
    assert(maxError === 0)
  }

  //todo - tests for 6 sort methods

  test("MatrixFunctions.argsort(matrix, context)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val context = Sorting.keyValueSortContext(Datatype.FLOAT, Datatype.INT, matrix.rows * matrix.columns, matrix.rows)
    val sortedIndices = MatrixFunctions.argsort(matrix, context)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeIndices = NativeOperations.argsort(nativeMatrix)

    val maxError = compare(sortedIndices.values(), sortedNativeIndices.data.map(_.map(_.toInt)))
    assert(maxError === 0)
  }

  test("MatrixFunctions.argsort(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val sortedIndices = MatrixFunctions.argsort(matrix)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeIndices = NativeOperations.argsort(nativeMatrix)

    val maxError = compare(sortedIndices.values(), sortedNativeIndices.data.map(_.map(_.toInt)))
    assert(maxError === 0)
  }

  //todo - tests for 2 argsort methods

  test("MatrixFunctions.bincount(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val bins = MatrixFunctions.bincount(matrix, 0)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.bincount(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val bins = MatrixFunctions.bincount(matrix, 1)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.bincount(matrix, maxValue, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
//    val data = Array(Array[Int](1, 2, 2, 3, 3, 3), Array[Int](4, 4, 4, 5, 5, 6))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val result = Matrix(matrix.columns, maxValue + 1).of(1)
    val bins = MatrixFunctions.bincount(matrix, maxValue, 0, result)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.bincount(matrix, maxValue, axis = 1, result)") {
        val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val result = Matrix(matrix.rows, maxValue + 1).of(1)
    val bins = MatrixFunctions.bincount(matrix, maxValue, 1, result)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.bincount(matrix, maxValue, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val bins = MatrixFunctions.bincount(matrix, maxValue, 0)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("MatrixFunctions.bincount(matrix, maxValue, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](2*ROWS, 2*COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
//    val data = NativeMatrix.matrixData[Float](1000, 50).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
//    val t1 = System.currentTimeMillis()
    val bins = MatrixFunctions.bincount(matrix, maxValue, 1)
//    val t2 = System.currentTimeMillis()
//    println(s"time: ${t2 - t1}")

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

}
