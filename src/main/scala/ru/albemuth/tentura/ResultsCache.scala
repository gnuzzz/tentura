package ru.albemuth.tentura

import scala.collection.mutable

/**
  * @author Vladimir Kornyshev { @literal <gnuzzz@mail.ru>}
  */
class ResultsCache {

  val functionsCaches = new mutable.HashMap[Any, mutable.HashMap[Any, Any]]()

  def result[R](function: AnyRef, args: Any, res: => R): R = {
    val functionCache = functionsCaches.getOrElseUpdate(function, new mutable.HashMap[Any, Any])
    functionCache.getOrElseUpdate(args, res).asInstanceOf[R]
  }

}
