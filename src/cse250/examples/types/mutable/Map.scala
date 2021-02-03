package cse250.examples.types.mutable

trait Map[K, V] {
  def addOne(elem: (K, V)): Unit

  def removeOne(key: K): Boolean

  def get(key: K): Option[V]

  def contains(key: K): Boolean = get(key).isDefined

  def iterator: Iterator[(K, V)]
}
