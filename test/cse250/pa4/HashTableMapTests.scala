/**
 * cse250.pa4.tests.HashTableMapTests.scala
 *
 * Copyright 2020 Andrew Hughes (ahughes6@buffalo.edu)
 *
 * This work is licensed under the Creative Commons
 * Attribution-NonCommercial-ShareAlike 4.0 International License.
 * To view a copy of this license, visit
 * http://creativecommons.org/licenses/by-nc-sa/4.0/.
 */

package cse250.pa4.tests

import cse250.pa4.HashTableMap
import org.scalatest.FlatSpec

class HashTableMapTests extends FlatSpec {
  val testSize = 100
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => ('A'+i).toChar.toString)
  behavior of "HashTableMap.insert"
  it should "add the (key,value) pairs" in {
    val hashMap = new HashTableMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      hashMap.addOne((k, v))
    }
    val iterator = hashMap.iterator
    val elementSet = collection.mutable.Set[(Int, String)]()
    for (_ <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      elementSet.add(elem)
    }
    for (i <- elements.indices) {
      val elem = elements(i)
      assert(elementSet.contains(elem))
    }
  }
}

