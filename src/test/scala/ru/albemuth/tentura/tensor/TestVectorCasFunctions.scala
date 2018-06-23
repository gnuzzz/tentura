package ru.albemuth.tentura.tensor

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.Comparator.Comparator
import ru.albemuth.tentura.tensor.Operator.Operator

import scala.util.Random

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestVectorCasFunctions extends FunSuite with TestUtils with TestWithResult {

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

  def points(length: Int): Array[Int] = {
    val allPoints = Random.shuffle(for (i <- 0 until length) yield i)
    val pointsLength = Random.nextInt(allPoints.length)
    allPoints.slice(0, pointsLength).toArray
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand: Float): Array[Float] = {
      for (value <- data) yield {
        calculate(value, comparator, threshold, operand)
      }
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, threshold, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operand), Vector.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Scalar, operand2: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand1: Float, operand2: Float): Array[Float] = {
      for (value <- data) yield {
        calculate(value, comparator, threshold, operand1, operand2)
      }
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, threshold, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Scalar, operand2: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operand1, operand2), Vector.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold, operand(i))
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, threshold, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operand), Vector.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Vector, operand2: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand1: Array[Float], operand2: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold, operand1(i), operand2(i))
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Vector, operand2: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operand1, operand2), Vector.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand: Float): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operand)
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, thresholdData, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operand), Vector.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: scalar, operand2: scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand1: Float, operand2: Float): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operand1, operand2)
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: scalar, operand2: scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operand1, operand2), Vector.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operand(i))
      }).toArray
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operand)
      val nativeResult = cas(data, comparator, thresholdData, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operand), Vector.Cas.cas(_, comparator, threshold, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Vector, operand2: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand1: Array[Float], operand2: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operand1(i), operand2(i))
      }).toArray
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operand1, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Vector, operand2: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operand1, operand2), Vector.Cas2.cas(_, comparator, threshold, operand1, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold, operand(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operand, indices)
      val nativeResult = cas(data, comparator, threshold, operandData, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Vector, operand2: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operand1: Array[Float], operand2: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold, operand1(i), operand2(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operand1, operand2, indices)
      val nativeResult = cas(data, comparator, threshold, operandData1, operandData2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operand1: Vector, operand2: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Scalar, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand: Float, indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operand)
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operand, indices)
      val nativeResult = cas(data, comparator, thresholdData, operand, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Scalar, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Scalar, operand2: Scalar, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand1: Float, operand2: Float, indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operand1, operand2)
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operand1, operand2, indices)
      val nativeResult = cas(data, comparator, thresholdData, operand1, operand2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Scalar, operand2: Scalar, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 100.4f

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operand(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operand, indices)
      val nativeResult = cas(data, comparator, thresholdData, operandData, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Vector, operand2: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operand1: Array[Float], operand2: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operand1(i), operand2(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operand1, operand2, indices)
      val nativeResult = cas(data, comparator, thresholdData, operandData1, operandData2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operand1: Vector, operand2: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operand1, operand2, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator: Operator, operand: Float): Array[Float] = {
      for (value <- data) yield {
        calculate(value, comparator, threshold, operator, operand)
      }
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, threshold, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operator, operand), Vector.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Scalar, operator2, operand2: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Float] = {
      for (value <- data) yield {
        calculate(value, comparator, threshold, operator1, operand1, operator2, operand2)
      }
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, threshold, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Scalar, operator2, operand2: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold, operator, operand(i))
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, threshold, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operator, operand), Vector.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold, operator1, operand1(i), operator2, operand2(i))
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Float): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operator, operand)
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operator, operand), Vector.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Scalar, operator1, operand1: Scalar)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operator1, operand1, operator2, operand2)
      }).toArray
    }

    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Scalar, operator2, operand2: Scalar, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operator, operand(i))
      }).toArray
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.cas(vector, comparator, threshold, operator, operand)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(COLUMNS)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.cas(_, comparator, threshold, operator, operand), Vector.Cas.cas(_, comparator, threshold, operator, operand, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float]): Array[Float] = {
      (for (i <- data.indices) yield {
        calculate(data(i), comparator, threshold(i), operator1, operand1(i), operator2, operand2(i))
      }).toArray
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val thresholdData = NativeVector.vectorData(COLUMNS)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(COLUMNS)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(COLUMNS)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2), Vector.Cas2.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator: Operator, operand: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold, operator, operand(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operator, operand, indices)
      val nativeResult = cas(data, comparator, threshold, operator, operandData, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator, operand: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Float, operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold, operator1, operand1(i), operator2, operand2(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2, indices)
      val nativeResult = cas(data, comparator, threshold, operator1, operandData1, operator2, operandData2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Scalar, operator1, operand1: Vector, operator2, operand2: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val threshold = data(0)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Scalar, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Float, indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operator, operand)
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operator, operand, indices)
      val nativeResult = cas(data, comparator, thresholdData, operator, operand, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Scalar, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand = 100.4f

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Scalar, operator2, operand2: Scalar, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Float, operator2: Operator, operand2: Float, indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operator1, operand1, operator2, operand2)
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2, indices)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operand1, operator2, operand2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Scalar, operator2, operand2: Scalar, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operand1 = 100.4f
    val operand2 = 321.7f

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator: Operator, operand: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operator, operand(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      val result = Vector.Cas.Indexed.cas(vector, comparator, threshold, operator, operand, indices)
      val nativeResult = cas(data, comparator, thresholdData, operator, operandData, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator, operand: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData = NativeVector.vectorData(indices.length)
    val operand = Vector.of(operandData)

    for (comparator <- Comparator.values; operator <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices), Vector.Cas.Indexed.cas(_, comparator, threshold, operator, operand, indices, _))
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector, indices: Vector)") {
    def cas(data: Array[Float], comparator: Comparator, threshold: Array[Float], operator1: Operator, operand1: Array[Float], operator2: Operator, operand2: Array[Float], indices: Array[Int]): Array[Float] = {
      val result = data.clone()
      for (i <- indices.indices) {
        result(indices(i)) = calculate(data(indices(i)), comparator, threshold(i), operator1, operand1(i), operator2, operand2(i))
      }
      result
    }
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      val result = Vector.Cas2.Indexed.cas(vector, comparator, threshold, operator1, operand1, operator2, operand2, indices)
      val nativeResult = cas(data, comparator, thresholdData, operator1, operandData1, operator2, operandData2, indicesData)
      val maxError = compare(result.values(), nativeResult)
      assert(maxError === 0, s"$comparator, $operator1, $operator2")
    }
  }

  test("cas(vector, comparator, threshold: Vector, operator1, operand1: Vector, operator2, operand2: Vector, indices: Vector, result: Vector)") {
    val data = NativeVector.vectorData(COLUMNS)
    val vector = Vector.of(data)
    val indicesData = points(vector.length)
    val indices = Vector.of(indicesData)
    val thresholdData = NativeVector.vectorData(indices.length)
    val threshold = Vector.of(thresholdData)
    val operandData1 = NativeVector.vectorData(indices.length)
    val operand1 = Vector.of(operandData1)
    val operandData2 = NativeVector.vectorData(indices.length)
    val operand2 = Vector.of(operandData2)

    for (comparator <- Comparator.values; operator1 <- Operator.values; operator2 <- Operator.values) {
      testWithResultV_Vf(vector, Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices), Vector.Cas2.Indexed.cas(_, comparator, threshold, operator1, operand1, operator2, operand2, indices, _))
    }
  }

}
