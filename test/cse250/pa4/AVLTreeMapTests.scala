/**
 * cse250.pa4.tests.AVLTreeMapTests.scala
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

package cse250.pa4.tests

import cse250.pa4.{AVLTree, AVLTreeMap}
import org.scalatest.FlatSpec


class AVLTreeMapTests extends FlatSpec {
  val testSize = 10
  val inputKeys = Array.tabulate(testSize)(i => i + 1)
  val inputValues = Array.tabulate(testSize)(i => ('A'+i).toChar.toString)
  behavior of "AVLTreeMap.insert"
  it should "add the (key,value) pairs" in {
    val treeMap = new AVLTreeMap[Int, String]
    val elements = inputKeys.zip(inputValues)
    for ((k, v) <- elements) {
      treeMap.addOne((k, v))
      assert(treeMap.contains(k))
    }
    val iterator = treeMap.iterator
    for (i <- elements.indices) {
      assert(iterator.hasNext)
      val elem = iterator.next
      assert(elem == elements(i))
    }
  }

  behavior of "Problem1.rotLeft."
  it should "rotate the tree left" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, true)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, true)
    node._right = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._right = node3
    val answer = treeObj.rotateLeft(node)
    assert(answer == node2)
    assert(answer._left == node)
    assert(answer._right == node3)
  }
  it should "rotate the tree left2" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, true)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, false)
    node._right = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._left = node3
    val node4 = new treeObj.AVLNode[(Int,String)]((4,"D"),null, null, node2, false, false)
    node2._right = node4
    val answer = treeObj.rotateLeft(node)
    assert(answer == node2)
    assert(answer._left == node)
    assert(answer._left._right == node3)
    assert(answer._right == node4)
  }

  behavior of "Problem1.rotRight."
  it should "rotate the tree right" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, true, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, true, false)
    node._left = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._left = node3
    val answer = treeObj.rotateRight(node)
    assert(answer == node2)
    assert(answer._left == node3)
    assert(answer._right == node)
  }
  it should "rotate the tree right2" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, true, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, false)
    node._left = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._left = node3
    val node4 = new treeObj.AVLNode[(Int,String)]((4,"D"),null, null, node2, false, false)
    node2._right = node4
    val answer = treeObj.rotateRight(node)
    assert(answer == node2)
    assert(answer._left == node3)
    assert(answer._right._left == node4)
    assert(answer._right == node)
  }

  behavior of "Problem1.rotLeftRight."
  it should "rotate the tree leftRight" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, true, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, true)
    node._left = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._right = node3
    val answer = treeObj.rotateLeftRight(node)

    assert(answer == node3)
    assert(answer._left == node2)
    assert(answer._right == node)
  }
  it should "rotate the tree leftRight2" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, true, false)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, true)
    node._left = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node, false, false)
    node._right = node3
    val node4 = new treeObj.AVLNode[(Int,String)]((4,"D"),null, null, node2, false, true)
    node2._right = node4
    val node5 = new treeObj.AVLNode[(Int,String)]((5,"E"),null, null, node4, false, false)
    node4._right = node5
    val answer = treeObj.rotateLeftRight(node)

    assert(answer == node4)
    assert(answer._left == node2)
    assert(answer._right == node)
    assert(answer._right._left == node5)
    assert(answer._right._right == node3)
  }

  behavior of "Problem1.rotRightLeft."
  it should "rotate the tree rightLeft" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, true)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, true, false)
    node._right = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node2, false, false)
    node2._left = node3
    val answer = treeObj.rotateRightLeft(node)

    assert(answer == node3)
    assert(answer._left == node)
    assert(answer._right == node2)
  }
  it should "rotate the tree rightLeft2" in {
    val treeObj = new AVLTree[Int,String]
    val node = new treeObj.AVLNode[(Int,String)]((1,"A"),null, null, null, false, true)
    val node2 = new treeObj.AVLNode[(Int,String)]((2,"B"),null, null, node, false, false)
    node._left = node2
    val node3 = new treeObj.AVLNode[(Int,String)]((3,"C"),null, null, node, true, false)
    node._right = node3
    val node4 = new treeObj.AVLNode[(Int,String)]((4,"D"),null, null, node3, true, false)
    node3._left = node4
    val node5 = new treeObj.AVLNode[(Int,String)]((5,"E"),null, null, node4, false, false)
    node4._left = node5
    val answer = treeObj.rotateRightLeft(node)

    assert(answer == node4)
    assert(answer._left == node)
    assert(answer._left._left == node2)
    assert(answer._left._right == node5)
    assert(answer._right == node3)
  }

  behavior of "insert"
  it should "insert" in {
    val treeObj = new AVLTree[Int,String]
    val one = treeObj.insert(1,"A")
    assert(one._value == (1,"A"))
    val two = treeObj.insert(3,"C")
    assert(two._value == (3,"C"))
    assert(two._parent._value == (1,"A"))
    assert(two._parent._right._value == (3,"C"))
    val three = treeObj.insert(2,"B")
    assert(three._value == (2,"B"))
    assert(three._parent == null)
    assert(three._left._value == (1,"A"))
    assert(three._right._value == (3,"C"))
    treeObj.remove(3)
    assert(three._value == (2,"B"))
    assert(three._left._value == (1,"A"))
    assert(three._right == null)
    assert(!treeObj.remove(5))
    //val four = treeObj.insert(4,"D")
    //assert(four._value == (4,"D"))
    //val five = treeObj.insert(5,"E")
    //assert(five._value == (5,"E"))
  }
}

