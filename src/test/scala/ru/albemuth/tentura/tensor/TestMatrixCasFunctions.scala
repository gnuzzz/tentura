package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Operator.Operator

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestMatrixCasFunctions extends FunSuite with TestUtils with TestWithResult {

  def calculate(value: Float, comparator: Comparator, threshold: Float, operand: Float): Float = {
    comparator match {
      case Comparator.== => if (value == threshold) operand else value
      case Comparator.!= => if (value != threshold) operand else value
      case Comparator.< =>  if (value < threshold) operand else value
      case Comparator.<= => if (value <= threshold) operand else value
      case Comparator.> =>  if (value > threshold) operand else value
      case Comparator.>= => if (value >= threshold) operand else value
    }
  }

  def calculate(value: Float, comparator: Comparator, threshold: Float, operand1: Float, operand2: Float): Float = {
    comparator match {
      case Comparator.== => if (value == threshold) operand1 else operand2
      case Comparator.!= => if (value != threshold) operand1 else operand2
      case Comparator.< =>  if (value < threshold) operand1 else operand2
      case Comparator.<= => if (value <= threshold) operand1 else operand2
      case Comparator.> =>  if (value > threshold) operand1 else operand2
      case Comparator.>= => if (value >= threshold) operand1 else operand2
    }
  }

  def calculate(value: Float, comparator: Comparator, threshold: Float, operator: Operator, operand: Float): Float = {
    comparator match {
      case Comparator.== => if (value == threshold) calculate(value, operator, operand) else value
      case Comparator.!= => if (value != threshold) calculate(value, operator, operand) else value
      case Comparator.< =>  if (value < threshold) calculate(value, operator, operand) else value
      case Comparator.<= => if (value <= threshold) calculate(value, operator, operand) else value
      case Comparator.> =>  if (value > threshold) calculate(value, operator, operand) else value
      case Comparator.>= => if (value >= threshold) calculate(value, operator, operand) else value
    }
  }

  def calculate(value: Float, comparator: Comparator, threshold: Float, operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Float = {
    comparator match {
      case Comparator.== => if (value == threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
      case Comparator.!= => if (value != threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
      case Comparator.< =>  if (value < threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
      case Comparator.<= => if (value <= threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
      case Comparator.> =>  if (value > threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
      case Comparator.>= => if (value >= threshold) calculate(value, operator1, operand1) else calculate(value, operator2, operand2)
    }
  }

  def calculate(value: Float, operator: Operator, operand: Float): Float = {
    operator match {
      case Operator.+ => value + operand
      case Operator.- => value - operand
      case Operator.* => value * operand
      case Operator./ => value / operand
      case Operator.ASSIGN => operand
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (value <- row) yield {
          calculate(value, comparator, threshold, operand)
        }
      }
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
//    val data = NativeMatrix.matrixData[Float](100000, 5000)
//    val warmMatrrix = new Matrix(1, 1)
//    Memory.print("aaa")
    val matrix = Matrix.of(data)
//    Memory.print("bbb")
    val threshold = data(0)(0)
    val operand = 100.4f

//    val result = Matrix.Cas.cas(matrix, Comparator.<=, threshold, operand)
//    val nativeResult = cas(data, Comparator.<=, threshold, operand)
//    println(s"${Runtime.getRuntime.totalMemory()}/${Runtime.getRuntime.freeMemory()}")
//    val maxError = compare(result.values(), nativeResult)
//    assert(maxError === 0, s"${Comparator.<=}")

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, threshold, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand), Matrix.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: Scalar, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand1: Float, operand2: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (value <- row) yield {
          calculate(value, comparator, threshold, operand1, operand2)
        }
      }
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    //    val data = NativeMatrix.matrixData[Float](100000, 5000)
    //    val warmMatrrix = new Matrix(1, 1)
    //    Memory.print("aaa")
    val matrix = Matrix.of(data)
    //    Memory.print("bbb")
    val threshold = data(0)(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    //    val result = Matrix.Cas.cas(matrix, Comparator.<=, threshold, operand)
    //    val nativeResult = cas(data, Comparator.<=, threshold, operand)
    //    println(s"${Runtime.getRuntime.totalMemory()}/${Runtime.getRuntime.freeMemory()}")
    //    val maxError = compare(result.values(), nativeResult)
    //    assert(maxError === 0, s"${Comparator.<=}")

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, threshold, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: Scalar, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operand(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 0)
      val nativeResult = cas(data, comparator, threshold, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: row, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operand1(j), operand2(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 0)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: row, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold, operand(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 1)
      val nativeResult = cas(data, comparator, threshold, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: column, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold, operand1(i), operand2(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 1)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: column, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, threshold, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand), Matrix.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: matrix, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand1: Array[Array[Float]], operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operandRow1(j), operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: matrix, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operand)
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: Scalar, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Float, operand2: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operand1, operand2)
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: Scalar, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operand)
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: Scalar, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Float, operand2: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operand1, operand2)
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: Scalar, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operand(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: row, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operand1(j), operand2(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: row, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operand(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: column, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operand1(i), operand2(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: column, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: matrix, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Array[Float]], operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operandRow1(j), operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operand1: matrix, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(i), operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: matrix, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Array[Float]], operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(i), operandRow1(j), operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operand1: matrix, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand)
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, thresholdData, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand), Matrix.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: Scalar, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand1: Float, operand2: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand1, operand2)
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: Scalar, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: row, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand1(j), operand2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: row, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand(i))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: column, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand1: Array[Float], operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operand1(i), operand2(i))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: column, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operand), Matrix.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: matrix, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand1: Array[Array[Float]], operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operandRow1(j), operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operand1: matrix, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2), Matrix.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold, operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operand, rows, columns)
      val nativeResult = cas(data, comparator, threshold, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operand1: Array[Float], operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold, operand1(i), operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operand1, operand2, rows, columns)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Scalar, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Float, rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand)
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operand, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Scalar, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Scalar, operand2: Scalar, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Float, operand2: Float, rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand1, operand2)
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operand1, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Scalar, operand2: Scalar, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Float], operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand1(i), operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operand1, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(rows(i))(columns(i)), operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operand1: Array[Float], operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(rows(i))(columns(i)), operand1(i), operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operand1, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operand1: Vector, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Matrix, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand: Array[Array[Float]], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand(rows(i))(columns(i)))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand: Matrix, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Matrix, operand2: Matrix, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operand1: Array[Array[Float]], operand2: Array[Array[Float]], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operand1(rows(i))(columns(i)), operand2(rows(i))(columns(i)))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operand1, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operand1: Matrix, operand2: Matrix, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator: Operator, operand: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (value <- row) yield {
          calculate(value, comparator, threshold, operator, operand)
        }
      }
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, threshold, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand), Matrix.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: Scalar, operator2, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (value <- row) yield {
          calculate(value, comparator, threshold, operator1, operand1, operator2, operand2)
        }
      }
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, threshold, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: Scalar, operator2, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operator, operand(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 0)
      val nativeResult = cas(data, comparator, threshold, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: row, operator2, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operator1, operand1(j), operator2, operand2(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: row, operator2, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold, operator, operand(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 1)
      val nativeResult = cas(data, comparator, threshold, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: column, operator2, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold, operator1, operand1(i), operator2, operand2(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: column, operator2, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operator, operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, threshold, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand), Matrix.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: matrix, operator2, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Array[Float]], operator2: Operator, operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold, operator1, operandRow1(j), operator2, operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: matrix, operator2, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val threshold = data(0)(0)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator, operand)
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: Scalar, operator2, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator1, operand1, operator2, operand2)
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: Scalar, operator2, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operator, operand)
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: Scalar, operator2, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operator1, operand1, operator2, operand2)
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: Scalar, operator2, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator, operand(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: row, operator2, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (row <- data) yield {
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator1, operand1(j), operator2, operand2(j))
        }
      }.toArray
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: row, operator2, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operator, operand(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: column, operator2, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        for (value <- row) yield {
          calculate(value, comparator, threshold(i), operator1, operand1(i), operator2, operand2(i))
        }
      }
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: column, operator2, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator, operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: matrix, operator2, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Array[Float]], operator2: Operator, operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(j), operator1, operandRow1(j), operator2, operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: row, operator1, operand1: matrix, operator2, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.columns)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(i), operator, operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: matrix, operator2, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Array[Float]], operator2: Operator, operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, threshold(i), operator1, operandRow1(j), operator2, operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: column, operator1, operand1: matrix, operator2, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeVector.vectorData(matrix.rows)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator: Operator, operand: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator, operand)
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand), Matrix.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: Scalar, operator2, operand2: Scalar)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator1, operand1, operator2, operand2)
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: Scalar, operator2, operand2: Scalar, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator, operand(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.columns)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: row, operator2, operand2: row)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator1, operand1(j), operator2, operand2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: row, operator2, operand2: row, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.columns)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.columns)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 0, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator: Operator, operand: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator, operand(i))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(matrix.rows)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1), Matrix.Cas.cas(_, comparator, threshold, operator, operand, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: column, operator2, operand2: column)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator1, operand1(i), operator2, operand2(i))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: column, operator2, operand2: column, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(matrix.rows)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(matrix.rows)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, axis = 1, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator: Operator, operand: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        val operandRow = operand(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator, operandRow(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.cas(matrix, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator, operand: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.cas(_, comparator, threshold, operator, operand), Matrix.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: matrix, operator2, operand2: matrix)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator1: Operator, operand1: Array[Array[Float]], operator2: Operator, operand2: Array[Array[Float]]): Array[Array[Float]] = {
      for (i <- data.indices) yield {
        val row = data(i)
        val thresholdRow = threshold(i)
        val operandRow1 = operand1(i)
        val operandRow2 = operand2(i)
        for (j <- row.indices) yield {
          calculate(row(j), comparator, thresholdRow(j), operator1, operandRow1(j), operator2, operandRow2(j))
        }
      }.toArray
    }.toArray
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: matrix, operator1, operand1: matrix, operator2, operand2: matrix, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Matrix.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold, operator, operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operator, operand, rows, columns)
      val nativeResult = cas(data, comparator, threshold, operator, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold, operator1, operand1(i), operator2, operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val threshold = data(0)(0)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Scalar, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Float, rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator, operand)
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operator, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Scalar, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Scalar, operator2, operand2: Scalar, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float, rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator1, operand1, operator2, operand2)
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Scalar, operator2, operand2: Scalar, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator, operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operator, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator1, operand1(i), operator2, operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operator, operand: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator: Operator, operand: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(rows(i))(columns(i)), operator, operand(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operator, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operator, operand: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData = NativeVector.vectorData(rows.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Array[Float]], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(rows(i))(columns(i)), operator1, operand1(i), operator2, operand2(i))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Matrix, operator1, operand1: Vector, operator2, operand2: Vector, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val threshold = Matrix.of(thresholdData)
    val operandData1 = NativeVector.vectorData(rows.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(rows.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Matrix, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Array[Float]], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator, operand(rows(i))(columns(i)))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Matrix.Cas.Indexed.cas(matrix, comparator, threshold, operator, operand, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator, operand: Matrix, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand = Matrix.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns), Matrix.Cas.Indexed.cas(_, comparator, threshold, operator, operand, rows, columns, _))
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Matrix, operator2, operand2: Matrix, rows: Vector, columns: Vector)") {
    def cas(data: Array[Array[Float]], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Array[Float]], operator2: Operator, operand2: Array[Array[Float]], rows: Array[Int], columns: Array[Int]): Array[Array[Float]] = {
      val result = (for (i <- data.indices) yield {
        data(i).clone()
      }).toArray
      for (i <- rows.indices) {
        result(rows(i))(columns(i)) = calculate(data(rows(i))(columns(i)), comparator, threshold(i), operator1, operand1(rows(i))(columns(i)), operator2, operand2(rows(i))(columns(i)))
      }
      result
    }
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Matrix.Cas2.Indexed.cas(matrix, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2, rowsData, columnsData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(matrix, comparator, threshold: Vector, operator1, operand1: Matrix, operator2, operand2: Matrix, rows: Vector, columns: Vector, result: matrix)") {
    val data = NativeMatrix.matrixData[Float](ROWS, COLUMNS)
    val matrix = Matrix.of(data)
    val (rowsData, columnsData) = points(matrix.rows, matrix.columns)
    val rows = Vector.of(rowsData)
    val columns = Vector.of(columnsData)
    val thresholdData = NativeVector.vectorData(rows.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand1 = Matrix.of(operandData1)
    val operandData2 = NativeMatrix.matrixData[Float](matrix.rows, matrix.columns)
    val operand2 = Matrix.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultM_M(matrix, Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns), Matrix.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, rows, columns, _))
    }
  }

}
