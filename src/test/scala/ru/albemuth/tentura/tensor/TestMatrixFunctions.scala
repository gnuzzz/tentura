package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.jcuda.jcusegsort.{Datatype, Sorting}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMatrixFunctions extends FunSuite with TestUtils with TestWithResult {

  test("Matrix.sum(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = Matrix.sum(a)
    val nativeResult = nativeA.sum()

    assert(java.lang.Math.abs(result.value() - nativeResult) / java.lang.Math.abs(nativeResult) < 0.0001)
  }

  test("Matrix.sum(matrix, result)") {
    testWithResultM_S(matrix(ROWS, COLUMNS), Matrix.sum(_), Matrix.sum(_, _))
  }

  test("Matrix.sumRows(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = Matrix.sum(a, axis = 0)
    val nativeResult = nativeA.sum(axis = 0)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("Matrix.sumRows(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.sumRows(_), Matrix.sumRows(_, _))
  }

  test("Matrix.sumColumns(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = Matrix.sum(a, axis = 1)
    val nativeResult = nativeA.sum(axis = 1)

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError === 0.0f)
  }

  test("Matrix.sumColumns(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.sumColumns(_), Matrix.sumColumns(_, _))
  }

  test("Matrix.mean(matrix)") {
    val nativeA = NativeMatrix.matrix(ROWS, COLUMNS)
    val a = Matrix.of(nativeA.data)

    val result = Matrix.mean(a)
    val nativeResult = nativeA.sum() / (nativeA.rows * nativeA.columns)

    val error = if (nativeResult == 0) java.lang.Math.abs(result.value() - nativeResult) else java.lang.Math.abs(result.value() - nativeResult) / java.lang.Math.abs(nativeResult)
    assert(error < 0.0001)
  }

  test("Matrix.mean(matrix, result)") {
    testWithResultM_S(matrix(ROWS, COLUMNS), Matrix.mean(_), Matrix.mean(_, _))
  }

  test("Matrix.rowsMean(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data)
    val a = Matrix.of(data)

    val result = Matrix.mean(a, axis = 1)
    val nativeResult = nativeA.sum(axis = 1) / nativeA.columns

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001) //different sum order in NativeMatrix.sum and Matrix.mean
  }

  test("Matrix.rowsMean(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.rowsMean(_), Matrix.rowsMean(_, _))
  }

  test("Matrix.columnsMean(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data)
    val a = Matrix.of(data)

    val result = Matrix.mean(a, axis = 0)
    val nativeResult = nativeA.sum(axis = 0) / nativeA.rows

    val maxError = compare(result.values(), nativeResult.data)
    assert(maxError < 0.0001) //different sum order in NativeMatrix.sum and Matrix.mean
  }

  test("Matrix.columnsMean(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.columnsMean(_), Matrix.columnsMean(_, _))
  }

  test("Matrix.meand(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(nativeA.data)

    val result = Matrix.meand(a)
    val nativeResult = nativeA.sum() / (nativeA.rows * nativeA.columns).toDouble

    val error = if (nativeResult == 0) java.lang.Math.abs(result.value() - nativeResult) else java.lang.Math.abs(result.value() - nativeResult) / java.lang.Math.abs(nativeResult)
    assert(error < 0.0001)
  }

  test("Matrix.meand(matrix, result)") {
    testWithResultM_Sd(matrix(ROWS, COLUMNS), Matrix.meand(_), Matrix.meand(_, _))
  }

  test("Matrix.rowsMeand(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(data)

    val result = Matrix.meand(a, axis = 1)
    val nativeResult = nativeA.sum(axis = 1) / nativeA.columns

    val maxError = compare(result.values(), nativeResult.data.map(_.toDouble))
    assert(maxError < 0.0001)
  }

  test("Matrix.rowsMeand(matrix, result)") {
    testWithResultM_Vd(matrix(ROWS, COLUMNS), Matrix.rowsMeand(_), Matrix.rowsMeand(_, _))
  }

  test("Matrix.columnsMeand(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(data)

    val result = Matrix.meand(a, axis = 0)
    val nativeResult = nativeA.sum(axis = 0) / nativeA.rows

    val maxError = compare(result.values(), nativeResult.data.map(_.toDouble))
    assert(maxError < 0.0001)
  }

  test("Matrix.columnsMeand(matrix, result)") {
    testWithResultM_Vd(matrix(ROWS, COLUMNS), Matrix.columnsMeand(_), Matrix.columnsMeand(_, _))
  }

  test("Matrix.std(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data)
    val a = Matrix.of(data)

    val result = Matrix.std(a)
    val mx2 = data.flatten.map(v => v * v).sum / data.flatten.length.toFloat
    val mx = data.flatten.sum / data.flatten.length.toFloat
    val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx)

    val error = if (nativeResult == 0) java.lang.Math.abs(result.value() - nativeResult) else java.lang.Math.abs(result.value() - nativeResult) / java.lang.Math.abs(nativeResult)
    assert(error < 0.0001)
  }

  test("Matrix.std(matrix, result)") {
    testWithResultM_S(matrix(ROWS, COLUMNS), Matrix.std(_), Matrix.std(_, _))
  }

  test("Matrix.rowsStd(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data)
    val a = Matrix.of(data)

    val result = Matrix.std(a, axis = 1)
    val mx2 = data.map(_.map(v => v * v).sum / nativeA.columns.toFloat)
    val mx = data.map(_.sum / nativeA.columns.toFloat)
    val nativeResult = mx2.zip(mx).map({case (v2, v) => java.lang.Math.sqrt(v2 - v * v).toFloat})

    val maxError = compare(result.values(), nativeResult)
    assert(maxError < 0.0001)
  }

  test("Matrix.rowsStd(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.rowsStd(_), Matrix.rowsStd(_, _))
  }

  test("Matrix.columnsStd(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data)
    val a = Matrix.of(data)

    val result = Matrix.std(a, axis = 0)
    val dataT = nativeA.t.data
    val mx2 = dataT.map(_.map(v => v * v).sum / nativeA.rows.toFloat)
    val mx = dataT.map(_.sum / nativeA.rows.toFloat)
    val nativeResult = mx2.zip(mx).map({case (v2, v) => java.lang.Math.sqrt(v2 - v * v).toFloat})

    val maxError = compare(result.values(), nativeResult)
    assert(maxError < 0.0001)
  }

  test("Matrix.columnsStd(matrix, result)") {
    testWithResultM_V(matrix(ROWS, COLUMNS), Matrix.columnsStd(_), Matrix.columnsStd(_, _))
  }

  test("Matrix.stdd(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(data)

    val result = Matrix.stdd(a)
    val mx2 = data.flatten.map(v => v * v).sum / data.flatten.length
    val mx = data.flatten.sum / data.flatten.length
    val nativeResult = java.lang.Math.sqrt(mx2 - mx * mx)

    val error = if (nativeResult == 0) java.lang.Math.abs(result.value() - nativeResult) else java.lang.Math.abs(result.value() - nativeResult) / java.lang.Math.abs(nativeResult)
    assert(error < 0.0001)
  }

  test("Matrix.stdd(matrix, result)") {
    testWithResultM_Sd(matrix(ROWS, COLUMNS), Matrix.stdd(_), Matrix.stdd(_, _))
  }

  test("Matrix.rowsStdd(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(data)

    val result = Matrix.stdd(a, axis = 1)
    val mx2 = data.map(_.map(v => v * v).sum / nativeA.columns)
    val mx = data.map(_.sum / nativeA.columns)
    val nativeResult = mx2.zip(mx).map({case (v2, v) => java.lang.Math.sqrt(v2 - v * v)})

    val maxError = compare(result.values(), nativeResult)
    assert(maxError < 0.0001)
  }

  test("Matrix.rowsStdd(matrix, result)") {
    testWithResultM_Vd(matrix(ROWS, COLUMNS), Matrix.rowsStdd(_), Matrix.rowsStdd(_, _))
  }

  test("Matrix.columnsStdd(matrix)") {
    val data = NativeMatrix.matrixData[Double](ROWS, COLUMNS)
    val nativeA = NativeMatrix(data.map(_.map(_.toFloat)))
    val a = Matrix.of(data)

    val result = Matrix.stdd(a, axis = 0)
    val dataT = nativeA.t.data
    val mx2 = dataT.map(_.map(v => v * v).sum / nativeA.rows)
    val mx = dataT.map(_.sum / nativeA.rows)
    val nativeResult = mx2.zip(mx).map({case (v2, v) => java.lang.Math.sqrt(v2 - v * v)})

    val maxError = compare(result.values(), nativeResult)
    assert(maxError < 0.0001)
  }

  test("Matrix.columnsStdd(matrix, result)") {
    testWithResultM_Vd(matrix(ROWS, COLUMNS), Matrix.columnsStdd(_), Matrix.columnsStdd(_, _))
  }

  test("Matrix.min(matrix, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Scalar[Float]()
    val min = Matrix.min(matrix, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min()

    assert(min.value() === nativeMin)
  }

  test("Matrix.min(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = Matrix.min(matrix)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min()

    assert(min.value() === nativeMin)
  }

  test("Matrix.min(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](COLUMNS)
    val min = Matrix.min(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 0)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("Matrix.min(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](ROWS)
    val min = Matrix.min(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 1)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("Matrix.min(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = Matrix.min(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 0)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("Matrix.min(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val min = Matrix.min(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeMin = nativeMatrix.min(axis = 1)

    val maxError = compare(min.values, nativeMin.data)
    assert(maxError === 0)
  }

  test("Matrix.max(matrix, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Scalar[Float]()
    val max = Matrix.max(matrix, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max()

    assert(max.value() === nativeMax)
  }

  test("Matrix.max(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val max = Matrix.max(matrix)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max()

    assert(max.value() === nativeMax)
  }

  test("Matrix.max(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](COLUMNS)
    val max = Matrix.max(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max(axis = 0)

    val maxError = compare(max.values, nativeMax.data)
    assert(maxError === 0)
  }

  test("Matrix.max(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Float](ROWS)
    val max = Matrix.max(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeMax = nativeMatrix.max(axis = 1)

    val maxError = compare(max.values, nativeMax.data)
    assert(maxError === 0)
  }

  test("Matrix.argmin(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](COLUMNS)
    val argmin = Matrix.argmin(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 0)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmin(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](ROWS)
    val argmin = Matrix.argmin(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 1)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmin(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmin = Matrix.argmin(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 0)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmin(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmin = Matrix.argmin(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmin = nativeMatrix.argmin(axis = 1)

    val maxError = compare(argmin.values, nativeArgmin.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmax(matrix, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](COLUMNS)
    val argmax = Matrix.argmax(matrix, axis = 0, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 0)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmax(matrix, axis = 1, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val result = new Vector[Int](ROWS)
    val argmax = Matrix.argmax(matrix, axis = 1, result)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 1)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmax(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmax = Matrix.argmax(matrix, axis = 0)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 0)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.argmax(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val argmax = Matrix.argmax(matrix, axis = 1)

    val nativeMatrix = NativeMatrix(data)
    val nativeArgmax = nativeMatrix.argmax(axis = 1)

    val maxError = compare(argmax.values, nativeArgmax.data.map(_.toInt))
    assert(maxError === 0)
  }

  test("Matrix.sort(matrix, context)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val context = Sorting.keySortContext(Datatype.FLOAT, matrix.rows * matrix.columns, matrix.rows)
    val sortedMatrix = Matrix.sort(matrix, context)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeMatrix = NativeOperations.sort(nativeMatrix)

    val maxError = compare(sortedMatrix.values(), sortedNativeMatrix.data)
    assert(maxError === 0)
  }

  test("Matrix.sort(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val sortedMatrix = Matrix.sort(matrix)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeMatrix = NativeOperations.sort(nativeMatrix)

    val maxError = compare(sortedMatrix.values(), sortedNativeMatrix.data)
    assert(maxError === 0)
  }

  //todo - tests for 6 sort methods

  test("Matrix.argsort(matrix, context)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val context = Sorting.keyValueSortContext(Datatype.FLOAT, Datatype.INT, matrix.rows * matrix.columns, matrix.rows)
    val sortedIndices = Matrix.argsort(matrix, context)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeIndices = NativeOperations.argsort(nativeMatrix)

    val maxError = compare(sortedIndices.values(), sortedNativeIndices.data.map(_.map(_.toInt)))
    assert(maxError === 0)
  }

  test("Matrix.argsort(matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val sortedIndices = Matrix.argsort(matrix)

    val nativeMatrix = NativeMatrix(data)
    val sortedNativeIndices = NativeOperations.argsort(nativeMatrix)

    val maxError = compare(sortedIndices.values(), sortedNativeIndices.data.map(_.map(_.toInt)))
    assert(maxError === 0)
  }

  //todo - tests for 2 argsort methods

  test("Matrix.bincount(matrix, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val bins = Matrix.bincount(matrix, 0)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("Matrix.bincount(matrix, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val bins = Matrix.bincount(matrix, 1)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("Matrix.bincount(matrix, maxValue, axis = 0, result)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
//    val data = Array(Array[Int](1, 2, 2, 3, 3, 3), Array[Int](4, 4, 4, 5, 5, 6))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val result = Matrix(matrix.columns, maxValue + 1).of(1)
    val bins = Matrix.bincount(matrix, maxValue, 0, result)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("Matrix.bincount(matrix, maxValue, axis = 1, result)") {
        val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val result = Matrix(matrix.rows, maxValue + 1).of(1)
    val bins = Matrix.bincount(matrix, maxValue, 1, result)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("Matrix.bincount(matrix, maxValue, axis = 0)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
    val bins = Matrix.bincount(matrix, maxValue, 0)

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix.t).t

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

  test("Matrix.bincount(matrix, maxValue, axis = 1)") {
    val data = NativeMatrix.matrixData[Float](2*ROWS, 2*COLUMNS).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
//    val data = NativeMatrix.matrixData[Float](1000, 50).map(_.map(_.toInt).map(v => if (v < 0) 0 else v))
    val matrix = Matrix.of(data)
    val maxValue = matrix.max().value()
//    val t1 = System.currentTimeMillis()
    val bins = Matrix.bincount(matrix, maxValue, 1)
//    val t2 = System.currentTimeMillis()
//    println(s"time: ${t2 - t1}")

    val nativeMatrix = NativeMatrix(data.map(_.map(_.toFloat)))
    val nativeBins = NativeOperations.bincount(nativeMatrix)

    val maxError = compare(bins.values().map(_.map(_.toFloat)), nativeBins.data)
    assert(maxError === 0)
  }

}
