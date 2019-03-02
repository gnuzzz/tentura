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

  def argsort(matrix: NativeMatrix): NativeMatrix = {
    NativeMatrix(matrix.data.map(_.zipWithIndex.sortBy(_._1).map(_._2.toFloat)))
  }

  def bincount(data: Array[Float], max: Float): Array[Float] = {
    val resultData = Array.ofDim[Float](max.toInt + 1)
    for (i <- data.indices) {
      resultData(data(i).toInt) += 1
    }
    resultData
  }

  def bincount(vector: NativeVector): NativeVector = {
    val max = vector.max()
    NativeVector(bincount(vector.data, max))
  }

  def bincount(matrix: NativeMatrix): NativeMatrix = {
    val max = matrix.max()
    NativeMatrix(matrix.data.map(bincount(_, max)))
  }

}
