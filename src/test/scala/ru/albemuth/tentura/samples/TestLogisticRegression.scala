package ru.albemuth.tentura.samples

import org.scalatest.FunSuite
import ru.albemuth.tentura.tensor.MathFunctions._
import ru.albemuth.tentura.tensor.Scalar._
import ru.albemuth.tentura.tensor.{Matrix, Vector}

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestLogisticRegression extends FunSuite {

  test("api") {
    val n = 100
    val features = 20
    val X: Matrix[Float] = new Matrix(n, features)
    val y: Vector[Float] = new Vector(n)
    val w: Vector[Float] = new Vector(features)

    val alpha = 0.01f
    val numEpochs = 10000
    var w_prev = w
    for (epoch <- 0 to numEpochs) {
//      val grad = X * (y - sigmoid(w_prev * X))
//      val w_next = w_prev - grad
      val w_next = w_prev + alpha * y * X * (1.0f - sigmoid((X * w) |* y))
      w_prev = w_next
    }
  }

}
