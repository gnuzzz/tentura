package ru.albemuth.tentura

import scala.collection.mutable

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ResultsCache {

  val functionsCaches = new mutable.HashMap[Any, mutable.HashMap[Any, DeviceVar[_]]]()

  def result[T, R <: DeviceVar[T]](function: AnyRef, args: Any, res: => R): R = {
    val functionCache = functionsCaches.getOrElseUpdate(function, new mutable.HashMap[Any, DeviceVar[_]])
    functionCache.getOrElseUpdate(args, res).asInstanceOf[R]
  }

}
