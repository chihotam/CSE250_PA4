/**
 * cse250.pa4.HashTableMap.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 *
 * Submission author
 * UBIT: chihotam
 * Person#: 50301678
 *
 * Collaborators (include UBIT name of each, comma separated):
 * UBIT:
 */
package cse250.pa4

import cse250.examples.types.mutable.Map
import scala.collection.mutable.ListBuffer
import scala.util.hashing.Hashing

class HashTableMap[K, V](val alphaMax: Double = 0.6)(implicit hash: Hashing[K]) extends Map[K, V] {
  var _n = 0
  var _N = 10
  var _alpha: Double = 0.0
  var _bucketArray = Array.fill[ListBuffer[(K, V)]](_N)(ListBuffer[(K, V)]())

  def rehash(newSize: Int): Unit = {
    if (newSize > _N) {
      val oldBucketArray = _bucketArray
      _n = 0
      _N = newSize
      _alpha = 0.0
      _bucketArray = Array.fill(_N)(ListBuffer[(K, V)]())
      for (bucket <- oldBucketArray; elem <- bucket) this.addOne(elem)
    }
  }

  override def get(key: K): Option[V] = {
    val lookupIndex = hash.hash(key) % _N
    _bucketArray(lookupIndex).find(elem => elem._1 == key) match {
      case Some(elem) => Some(elem._2)
      case None       => None
    }
  }

  override def addOne(elem: (K, V)): Unit = {
    val key = hash.hash(elem._1) % _N
    var found = false
    for(x <- _bucketArray(key).indices){
      if(_bucketArray(key)(x)._1 == elem._1){
        _bucketArray(key)(x) = elem
        found = true
      }
    }
    if(!found)
      if((_n+1).toDouble/_N.toDouble > alphaMax){
        rehash(2*_N)
      }
      elem +=: _bucketArray(key)
      _n += 1
      _alpha = _n.toDouble/_N.toDouble
    }
 override def removeOne(key: K): Boolean = {
    val keyz = hash.hash(key) % _N
    for(x <- _bucketArray(keyz).indices){
      if(_bucketArray(keyz)(x)._1 == key){
        _bucketArray(keyz) -= _bucketArray(keyz)(x)
        _n -= 1
        _alpha = _n.toDouble/_N.toDouble
        return true
      }
    }
    false
  }

  override def iterator: Iterator[(K, V)] = new Iterator[(K,V)]{
    var list = 0
    var index = 0
    var total = 0
    var current: (K,V) = null
    if(_n > 0){
      while(_bucketArray(list).isEmpty){
        list += 1
      }
      current = _bucketArray(list)(index)
      index += 1
    }

    def hasNext: Boolean = {
      if(current != null){
        true
      }
      else{
        false
      }
    }

    def next(): (K, V) = {
      val temp = current
      var break = false
      total += 1
      if(total == _n){
        break = true
        current = null
      }
      if(!break) {
        if(index < _bucketArray(list).length){
          current = _bucketArray(list)(index)
        }
        else{
          list += 1
          index = 0
          while(_bucketArray(list).isEmpty){
            list += 1
          }
          current = _bucketArray(list)(index)
        }
        index += 1
      }
      temp
    }
  }
}