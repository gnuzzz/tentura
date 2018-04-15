package ru.albemuth.tentura.tensor

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
object NativeOperations {

  def sort(vector: NativeVector): NativeVector = {
    NativeVector(vector.data.sorted)
  }

  def sort(matrix: NativeMatrix): NativeMatrix = {
    NativeMatrix(matrix.data.map(_.sorted))
  }

  def argsort(vector: NativeVector): NativeVector = {
    NativeVector(vector.data.zipWithIndex.sortBy(_._1).map(_._2.toFloat))
  }

  def argsort(vector: NativeVector, args: NativeVector): NativeVector = {
    NativeVector(vector.data.zip(args.data).sortBy(_._1).map(_._2))
  }

  def bincount(vector: NativeVector): NativeVector = {
    val max = vector.max()
    val resultData = Array.ofDim[Float](max.toInt + 1)
    for (i <- vector.data.indices) {
      resultData(vector.data(i).toInt) += 1
    }
    NativeVector(resultData)
  }

}
