package ru.albemuth.tentura.performance

import org.scalatest.FunSuite
import ru.albemuth.tentura.performance.SVM.{array2DataArray, str}
import ru.albemuth.tentura.tensor.Scalar._
import ru.albemuth.tentura.tensor.{Comparator, Math, Matrix, NativeMatrix, TestUtils, Vector}

import scala.reflect.ClassTag
import scala.util.Random

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class TestPerf extends FunSuite with TestUtils {

  test("perf") {
    val X_dev = new Matrix[Float](500, 3073)
    val y_dev = new Vector[Int](500)
    val W = Matrix[Float](3073, 10).of(Random.nextGaussian().toFloat) * 0.0001f
    val t1 = System.nanoTime()
    val (loss_vectorized, _) = SVM.svm_loss_vectorized(W, X_dev, y_dev, 0.000005f)
    val t2 = System.nanoTime()
    println(s"First vectorized loss: $loss_vectorized computed in ${t2 - t1}ns")
    val t3 = System.nanoTime()
    val (loss_vectorized2, _) = SVM.svm_loss_vectorized(W, X_dev, y_dev, 0.000005f)
    val t4 = System.nanoTime()
    println(s"Second vectorized loss: $loss_vectorized2 computed in ${t4 - t3}ns")

    val x_train_data = new NDArray[Float](Array.ofDim[Float](50000 * 3073), Array(50000, 3073))
    val y_train_data = new NDArray[Int](Array.ofDim[Int](50000), Array(50000))
    val svm = new SVM(x_train_data.shape(1), y_train_data.max() + 1)
    val t5 = System.nanoTime()
//    val loss_hist = svm.train(x_train_data, y_train_data, learning_rate = 1e-7f, reg = 2.5e4f, num_iters=1500, verbose=false)
    val loss_hist = svm.train(x_train_data, y_train_data, learning_rate = 1e-7f, reg = 2.5e4f, num_iters=5000000, verbose=true)
    val t6 = System.nanoTime()
    println(s"Loss achieved ${loss_hist.last}, that took ${t6 - t5}ns")
  }

}

class SVM(val num_features: Int, val num_classes: Int) {

  val W: Matrix[Float] = Matrix(num_features, num_classes).of(Random.nextGaussian().toFloat) * 0.001f

  def train(X: NDArray[Float], y: NDArray[Int], learning_rate: Float = 1e-3f, reg: Float = 1e-5f, num_iters: Int = 100, batch_size: Int = 200, verbose: Boolean = false): Seq[Float] = {
    val num_train = X.shape(0)

    val X_batch_data = Array.ofDim[Float](batch_size * X.rowLength)
    val y_batch_data = Array.ofDim[Int](batch_size * y.rowLength)
    val X_batch = new Matrix[Float](batch_size, num_features)
    val y_batch = new Vector[Int](batch_size)
    val range = Array.range(0, num_train)
    val sampler = new BatchSampler[Int](range, batch_size)
    val loss_history = for (it <- 0 until num_iters) yield {
      //      DataUtils.shuffle(range)
      //      val indices = range.slice(0, batch_size)
      val indices = sampler.nextBatch()
      //      val xx = X(indices, 0, X_batch_data).data
      //      val yy = y(indices, 0, y_batch_data).data

//      X_batch.copy2device(X(indices, 0, X_batch_data).data)
//      y_batch.copy2device(y(indices, 0, y_batch_data).data)
      //      X_batch.copy2device(xx)
      //      y_batch.copy2device(yy)
      //
      val (current_loss, grad) = loss(X_batch, y_batch, reg)
      W -= learning_rate * grad
      if (verbose && it % 10000 == 0) {
        println(s"iteration $it / $num_iters: loss $current_loss")
      }
      //      (current_loss, grad)
      current_loss
      //      0f
    }

    loss_history
  }

  def loss(X_batch: Matrix[Float], y_batch: Vector[Int], reg: Float): (Float, Matrix[Float]) = SVM.svm_loss_vectorized(W, X_batch, y_batch, reg)

}

object SVM {

  def svm_loss_vectorized(W: Matrix[Float], X: Matrix[Float], y: Vector[Int], reg: Float): (Float, Matrix[Float]) = {
    val num_train = X.rows
    val scores = X *** W
    val correct_class_scores = scores(y, axis = 0)
    val margins = Math.max[Float](scores -| correct_class_scores + 1.0f, 0)
    margins(y, axis = 0) = 0
    val loss = margins.sum(axis = 1).mean() + reg * (W * W).sum()

    val binary = Matrix.Cas.cas[Float](margins, Comparator.>, 0, 1)
    val mc = binary.sum(axis = 1)
    binary(y, axis = 0) -= mc
    val dW = X.T *** binary / num_train.toFloat + reg * 2.0f * W
    (loss, dW)
  }

  implicit def array2DataArray[T: ClassTag](array: Array[T]): DataArray[T] = new DataArray[T](array)

  def str[T](array: Array[T]): String = {
    val sb = new StringBuilder
    sb.append("(")
    for (item <- array) {
      if (sb.size > 1) sb.append(", ")
      item match {
        case items: Array[_] =>
          sb.append(str(items))
        case _ =>
          sb.append(item.toString)
      }
    }
    sb.append(")")
    sb.toString()
  }

}

class BatchSampler[T: ClassTag](items: Array[T], val batch_size: Int) {

  private var _items: IndexedSeq[T] = items
  private var _epoch = -1
  private var batch_start = -1

  def epoch: Int = _epoch

  def nextBatch(): Array[T] = {
    if (batch_start < 0) {
      _items = Random.shuffle[T, IndexedSeq](_items)
      _epoch += 1
      batch_start = 0
    }
    val batch = if (batch_size < _items.size) _items.slice(batch_start, batch_size) else _items
    batch_start += batch_size
    if (batch_start + batch_size >= _items.size) batch_start = -1
    batch.toArray
  }
}

class DataArray[T: ClassTag](val array: Array[T]) {

  def deepClone(): Array[T] = {
    DataArray.deepClone(array)
  }

  def get[ITEM: ClassTag](index: Array[Int]): ITEM = {
    DataArray.get[ITEM](array, index)
  }

  def set[ITEM: ClassTag](index: Array[Int], value: ITEM): Unit = {
    DataArray.set[ITEM](array, index, value)
  }

  def shape(): Array[Int] = {
    def shape(item: Any, sh: List[Int]): List[Int] = {
      item match {
        case items: Array[_] =>
          if (items.length == 0) {
            sh
          } else {
            shape(items(0), sh :+ items.length)
          }
        case _ =>
          sh
      }
    }
    shape(array, List()).toArray
  }

  def nextIndex(shape: Array[Int], index: Array[Int]): Array[Int] = {
    def nextIndex(shape: Array[Int], dim: Int, next: Array[Int]): Array[Int] = {
      val idx = next(dim) + 1
      if (idx < shape(dim)) {
        next(dim) = idx
        next
      } else if (dim == 0) {
        throw new IndexOutOfBoundsException(s"Can't create next index for index ${str(index)} and shape ${str(shape)}")
      } else {
        next(dim) = 0
        nextIndex(shape, dim - 1, next)
      }
    }
    if (shape.length != index.length) throw new IllegalArgumentException(s"Index ${str(index)} does not match shape ${str(shape)}")
    nextIndex(shape, shape.length - 1, index.clone())
  }

  def nextIndex(index: Array[Int]): Array[Int] = {
    nextIndex(shape(), index)
  }

  def reshape[ITEM: ClassTag](newShape: Array[Int]): Array[_] = {
    def fill(values: Array[_], shape: Array[Int], index: Array[Int], reshaped: Array[_], reshapedShape: Array[Int], reshapedIndex: Array[Int], itemIndex: Int, itemsNumber: Int): Unit = {
      val value = DataArray.get[ITEM](values, index)
      DataArray.set[ITEM](reshaped, reshapedIndex, value)
      if (itemIndex < itemsNumber - 1) {
        fill(values, shape, nextIndex(shape, index), reshaped, reshapedShape, nextIndex(reshapedShape, reshapedIndex), itemIndex + 1, itemsNumber)
      }
    }

    val currentShape = shape()
    val itemsNumber = currentShape.product
    if (newShape.product != itemsNumber) throw new IllegalArgumentException(s"New shape ${str(newShape)} does not match current shape ${str(currentShape)}")
    val reshaped = DataArray.ofDim[ITEM](newShape)
    fill(array, currentShape, Array.ofDim[Int](currentShape.length), reshaped, newShape, Array.ofDim[Int](newShape.length), 0, itemsNumber)
    reshaped
  }

}

object DataArray {

  def deepClone[T: ClassTag](array: Array[T]): Array[T] = {
    val cloned = array.clone()
    for (i <- array.indices) {
      val item = array(i)
      item match {
        case items: Array[_] =>
          cloned(i) = deepClone(items)(ClassTag(items.getClass.getComponentType)).asInstanceOf[T]
        case _ => //do nothing
      }
    }
    cloned
  }

  def ofDim[T: ClassTag](shape: Array[Int]): Array[_] = {
    def array(shape: Array[Int], dim: Int): Array[_] = {
      if (dim == shape.length - 1) {
        Array.ofDim[T](shape(dim))
      } else {
        val firstItem = array(shape, dim + 1)
        Array.fill(shape(dim))(deepClone(firstItem))(ClassTag(firstItem.getClass))
      }
    }
    array(shape, 0)
  }

  def get[ITEM: ClassTag](array: Array[_], index: Array[Int]): ITEM = {
    def get(items: Array[_], index: Array[Int], dim: Int): ITEM = {
      if (dim == index.length - 1) {
        items.asInstanceOf[Array[ITEM]](index(dim))
      } else {
        get(items(index(dim)).asInstanceOf[Array[_]], index, dim + 1)
      }
    }
    get(array, index, 0)
  }

  def set[ITEM: ClassTag](array: Array[_], index: Array[Int], value: ITEM): Unit = {
    def set(items: Array[_], index: Array[Int], dim: Int, value: ITEM): Unit = {
      if (dim == index.length - 1) {
        items.asInstanceOf[Array[ITEM]](index(dim)) = value
      } else {
        set(items(index(dim)).asInstanceOf[Array[_]], index, dim + 1, value)
      }
    }
    set(array, index, 0, value)
  }

}

class NDArray[T: ClassTag](val data: Array[T], val shape: Array[Int]) {

  val rowLength: Int = {
    var product = 1
    for (i <- 1 until shape.length) product *= shape(i)
    product
  }

  def apply(indices: Array[Int], dim: Int, resultData: Array[T]): NDArray[T] = {
    if (dim == 0) {
      for (i <- indices.indices) {
        System.arraycopy(data, indices(i) * rowLength, resultData, i * rowLength, rowLength)
      }
      NDArray(resultData, (List(indices.length) ++ shape.slice(1, shape.length)).toArray)
    } else {
      ???
    }
  }

  def slice(from: Int, to: Int, dim: Int): NDArray[T] = {
    if (dim == 0) {
      val rowLength = shape.slice(1, shape.length).product
      new NDArray[T](data.slice(from * rowLength, to * rowLength), (List(to - from) ++ shape.slice(1, shape.length)).toArray)
    } else {
      ???
    }
  }

  def reshape(newShape: Array[Int]): NDArray[T] = {
    if (newShape.product != shape.product) throw new IllegalArgumentException(s"New shape ${str(newShape)} does not match current shape ${str(shape)}")
    NDArray(data, newShape)
  }

  def mean(dim: Int): NDArray[Float] = {
    if (dim == 0) {
      if (shape.length == 2) {
        val result = NDArray[Float](Array.ofDim[Float](shape(1)), shape.slice(1, shape.length))
        val clazz = implicitly[ClassTag[T]].runtimeClass
        clazz match {
          case b if b == classOf[Boolean] => ???
          case b if b == classOf[Byte] => ???
          case c if c == classOf[Char] => ???
          case s if s == classOf[Short] => ???
          case i if i == classOf[Int] =>
            for (
              i <- 0 until shape (0);
              j <- 0 until shape (1)
            ) {
              val value = data (i * shape (1) + j).asInstanceOf[Int]
              val resultValue = result.data(j)
              result.data (j) = (value + i * resultValue) / (i + 1).asInstanceOf[Float]
            }
          case l if l == classOf[Long] => ???
          case f if f == classOf[Float] =>
            for (
              i <- 0 until shape (0);
              j <- 0 until shape (1)
            ) {
              val value = data (i * shape (1) + j).asInstanceOf[Float]
              val resultValue = result.data (j)
              result.data (j) = (value + i * resultValue) / (i + 1)
            }
          case d if d == classOf[Double] => ???
          case _ => ??? //not supported
        }
        result
      } else {
        ???
      }
    } else {
      ???
    }
  }

  def max()(implicit ord: Ordering[T]): T = {
    data.max
  }

  def -(array: NDArray[T]): NDArray[T] = {
    val result = NDArray[T](data.deepClone(), shape)
    if (shape.length == 2) {
      val clazz = implicitly[ClassTag[T]].runtimeClass
      clazz match {
        case b if b == classOf[Boolean] => ???
        case b if b == classOf[Byte] => ???
        case c if c == classOf[Char] => ???
        case s if s == classOf[Short] => ???
        case i if i == classOf[Int] =>
          for (
            i <- 0 until shape (0);
            j <- 0 until shape (1)
          ) {
            val value = data (i * shape (1) + j).asInstanceOf[Int]
            val r = result.data(j)
            result.asInstanceOf[NDArray[Int]].data(i * shape (1) + j) = value - array.data(j).asInstanceOf[Int]
          }
          result
        case l if l == classOf[Long] => ???
        case f if f == classOf[Float] =>
          for (
            i <- 0 until shape (0);
            j <- 0 until shape (1)
          ) {
            val value = data(i * shape (1) + j).asInstanceOf[Float]
            result.asInstanceOf[NDArray[Float]].data(i * shape (1) + j) = value - array.data(j).asInstanceOf[Float]
          }
          result
        case d if d == classOf[Double] => ???
        case _ => ??? //not supported
      }
    } else {
      ???
    }
  }

  def addColumn(value: => T): NDArray[T] = {
    if (shape.length == 2) {
      val resultData = Array.ofDim[T](shape(0) * (shape(1) + 1))
      val resultShape = Array(shape(0), shape(1) + 1)
      for (i <- 0 until shape(0)) {
        System.arraycopy(data, i * shape(1), resultData, i * resultShape(1), shape(1))
        resultData(i * resultShape(1) + resultShape(1) - 1) = value
      }
      NDArray(resultData, resultShape)
    } else {
      ???
    }
  }

}

object NDArray {

  def apply[T: ClassTag](data: Array[T], shape: Array[Int]): NDArray[T] = {
    new NDArray[T](data, shape)
  }

  def apply[T: ClassTag](array: Array[_]): NDArray[T] = {
    def fill(data: Array[T], index: Int, items: Array[_], itemIndex: Int): Int = {
      val item = items(itemIndex)
      item match {
        case values: Array[_] =>
          var dataIndex = index
          var i = 0
          while (i < values.length) {
            dataIndex = fill(data, dataIndex, values, i)
            i = i + 1
          }
          dataIndex
        case _ =>
          data(index) = item.asInstanceOf[T]
          index + 1
      }
    }
    val shape = array.shape()
    val data = Array.ofDim[T](shape.product)
    fill(data, 0, Array(array), 0)
    NDArray(data, shape)
  }

}