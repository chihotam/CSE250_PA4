/**
 * cse250.pa4.AVLTreeMap.scala
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

import collection.mutable.{ArrayStack, Stack}

class AVLTreeMap[K, V]()(implicit ord: Ordering[K]) extends Map[K, V]{
  val _storageTree = new AVLTree[K, V]

  override def addOne(elem: (K, V)): Unit = _storageTree.insert(elem)

  override def removeOne(key: K): Boolean = _storageTree.remove(key)

  override def get(key: K): Option[V] = _storageTree.find(key) match {
    case n: _storageTree.AVLNode[(K, V)] if n != null => Some(n._value._2)
    case null                                         => None
  }

  override def iterator: Iterator[(K, V)] = _storageTree.iterator
}

class AVLTree[K, V]()(implicit ord: Ordering[K]) {

  class AVLNode[A](var _value: A, var _left: AVLNode[A], var _right: AVLNode[A], var _parent: AVLNode[A],
                   var _leftH: Boolean, var _rightH: Boolean)

  var _avlRoot: AVLNode[(K, V)] = null

  def find(elem: K): AVLNode[(K, V)] = {
    var current = _avlRoot
    var found = false
    while (!found && current != null) {
      val currentKey = current._value._1
      if (ord.lt(elem, currentKey)) current = current._left
      else if (ord.lt(currentKey, elem)) current = current._right
      else found = true
    }
    current
  }

  def rotateLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val A: AVLNode[(K, V)] = nodeA
    val B: AVLNode[(K, V)] = nodeA._right

    if(B._left != null){
      A._right = B._left
      B._left._parent = A
    }
    else{
      A._right = null
    }
    B._left = A
    B._leftH = false
    B._rightH = false
    B._parent = A._parent
    A._leftH = false
    A._rightH = false
    A._parent = B

    B
  }

  def rotateRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val A: AVLNode[(K, V)] = nodeA
    val B: AVLNode[(K, V)] = nodeA._left

    if(B._right != null){
      A._left = B._right
      B._right._parent = A
    }
    else{
      A._left = null
    }
    B._right = A
    B._leftH = false
    B._rightH = false
    B._parent = A._parent
    A._leftH = false
    A._rightH = false
    A._parent = B

    B
  }

  def rotateLeftRight(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val A: AVLNode[(K, V)] = nodeA
    val B: AVLNode[(K, V)] = nodeA._left
    val C: AVLNode[(K, V)] = nodeA._left._right

    if(C._left != null){
      B._right = C._left
      C._left._parent = B._right
    }
    else{
      B._right = null
    }
    if(C._right != null){
      A._left = C._right
      C._right._parent = A._left
    }
    else{
      A._left = null
    }
    C._left = B
    C._right = A
    C._parent = A._parent
    B._parent = C
    A._parent = C
    if(!C._leftH && !C._rightH){
      A._leftH = false
      A._rightH = false
      B._leftH = false
      B._rightH = false
    }
    else if(C._leftH){
      A._leftH = false
      A._rightH = true
      B._leftH = false
      B._rightH = false
    }
    else{
      A._leftH = false
      A._rightH = false
      B._leftH = true
      B._rightH = false
    }
    C._leftH = false
    C._rightH = false

    C
  }

  def rotateRightLeft(nodeA: AVLNode[(K, V)]): AVLNode[(K, V)] = {
    val A: AVLNode[(K, V)] = nodeA
    val B: AVLNode[(K, V)] = nodeA._right
    val C: AVLNode[(K, V)] = nodeA._right._left

    if(C._left != null){
      A._right = C._left
      C._left._parent = A._right
    }
    else{
      A._right = null
    }
    if(C._right != null){
      B._left = C._right
      C._right._parent = B._left
    }
    else{
      B._left = null
    }
    C._left = A
    C._right = B
    C._parent = A._parent
    A._parent = C
    B._parent = C
    if(!C._leftH && !C._rightH){
      A._leftH = false
      A._rightH = false
      B._leftH = false
      B._rightH = false
    }
    else if(C._leftH){
      A._leftH = false
      A._rightH = false
      B._leftH = false
      B._rightH = true
    }
    else{
      A._leftH = true
      A._rightH = false
      B._leftH = false
      B._rightH = false
    }
    C._leftH = false
    C._rightH = false

    C
  }

  def balance(node: AVLNode[(K, V)]): Unit = {
    var current: AVLNode[(K, V)] = node._parent
    var num = 0
    var break = false
    while(!break){
      if(current._leftH){
        num += 1
        if(num == 2){
          if(current._left._leftH){
            if(current._parent == null){
              _avlRoot = rotateRight(current)
            }
            else{
              if(current == current._parent._left){
                current._parent._left = rotateRight(current)
              }
              else{
                current._parent._right = rotateRight(current)
              }
            }
          }
          else{
            if(current._parent == null){
              _avlRoot = rotateLeftRight(current)
            }
            else{
              if(current == current._parent._left){
                current._parent._left = rotateLeftRight(current)
              }
              else{
                current._parent._right = rotateLeftRight(current)
              }
            }
          }
          break = true
        }
      }
      else if(current._rightH){
        num += 1
        if(num == 2){
          if(current._right._rightH){
            if(current._parent == null){
              _avlRoot = rotateLeft(current)
            }
            else{
              if(current == current._parent._left){
                current._parent._left = rotateLeft(current)
              }
              else{
                current._parent._right = rotateLeft(current)
              }
            }
          }
          else{
            if(current._parent == null){
              _avlRoot = rotateRightLeft(current)
            }
            else{
              if(current == current._parent._left){
                current._parent._left = rotateRightLeft(current)
              }
              else{
                current._parent._right = rotateRightLeft(current)
              }
            }          }
          break = true
        }
      }
      else{
        break = true
      }
      current = current._parent
      if(current == null){
        break = true
      }
    }
  }

  def insert(elem: (K, V)): AVLNode[(K, V)] = {
    var current: AVLNode[(K, V)] = _avlRoot
    var next: AVLNode[(K, V)] = _avlRoot
    var side = 0
    if(current == null){
      _avlRoot = new AVLNode[(K,V)](elem,null,null,null,false,false)
      return find(elem._1)
    }
    while(next != null){
      current = next
      if(ord.lt(elem._1,current._value._1)){
        next = current._left
        side = -1
      }
      else if(ord.lt(current._value._1,elem._1)){
        next = current._right
        side = 1
      }
      else{
        current._value = elem
        return current
      }
    }
    val insert = new AVLNode[(K,V)](elem,null,null,current,false,false)
    if(side == -1){
      current._left = insert
      current._leftH = true
    }
    else{
      current._right = insert
      current._rightH = true
    }
    balance(insert)
    find(elem._1)
  }

  def remove(key: K): Boolean = {
    val node: AVLNode[(K,V)] = find(key)
    if(node == null){
      false
    }
    else{
      if(node._left == null && node._right == null){
        if(node._parent == null){
          _avlRoot = null
        }
        else{
          if(node == node._parent._left){
            node._parent._left = null
            if(node._parent._leftH){
              node._parent._leftH = false
            }
          }
          else{
            node._parent._right = null
            if(node._parent._rightH){
              node._parent._rightH = false
            }
          }
        }
        balance(node)
      }
      else if(node._left == null){
        node._value = node._right._value
        node._left = node._right._left
        node._right = node._right._right
        node._leftH = node._right._leftH
        node._rightH = node._right._rightH
        balance(node)
      }
      else if(node._right == null){
        node._value = node._left._value
        node._left = node._left._left
        node._right = node._left._right
        node._leftH = node._left._leftH
        node._rightH = node._left._rightH
        balance(node)
      }
      else{
        var temp: AVLNode[(K,V)] = node._right
        while(temp._left != null){
          temp = temp._left
        }
        node._value = temp._value
        if(temp == temp._parent._left){
          temp._parent._left = null
          if(temp._parent._leftH){
            temp._parent._leftH = false
          }
        }
        else{
          temp._parent._right = null
          if(temp._parent._rightH){
            temp._parent._rightH = false
          }
        }
        remove(key)
      }
      true
    }
  }

  def iterator: Iterator[(K, V)] = new Iterator[(K, V)] {
    val _parentStack = {
      val stack = new Stack[AVLNode[(K, V)]]
      var currentNode = _avlRoot
      while (currentNode != null) {
        stack.push(currentNode)
        currentNode = currentNode._left
      }
      stack
    }

    override def hasNext: Boolean = _parentStack.nonEmpty

    override def next(): (K, V) = {
      val originalTop = _parentStack.top
      if (originalTop._right != null) {
        var currentNode = originalTop._right
        while (currentNode != null) {
          _parentStack.push(currentNode)
          currentNode = currentNode._left
        }
      }
      else {
        var recentTop = _parentStack.pop
        while (_parentStack.nonEmpty && recentTop != _parentStack.top._left) {
          recentTop = _parentStack.pop
        }
      }
      originalTop._value
    }
  }
}
