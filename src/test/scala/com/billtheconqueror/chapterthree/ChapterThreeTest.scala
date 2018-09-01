package com.billtheconqueror.chapterthree

import org.scalatest.FunSuite

class ChapterThreeTest extends FunSuite {

  test("Exercise 3.1 - verify my answer") {
    assert(List.x == 3)
  }

  test("Exercise 3.2 - def tail[A](l: List[A]): List[A]") {
    assert(Nil == List.tail(Nil))
    assert(List(2, 3, 4) == List.tail(List(1, 2, 3, 4)))
  }

  test("Exercise 3.3 - def setHead[A](l: List[A], nh: A): List[A]") {
    assert(List(1) == List.setHead(Nil, 1))
    assert(List(1, 2, 3, 4) == List.setHead(List(2, 2, 3, 4), 1))
  }

  test("Exercise 3.4 - def drop[A](l: List[A], n: Int): List[A]") {
    assert(List(4, 5) == List.drop(List(1, 2, 3, 4, 5), 3))
    assert(List(1, 2, 3, 4, 5) == List.drop(List(1, 2, 3, 4, 5), 0))
    assert(Nil == List.drop(Nil, 5))
  }

  test("Exercise 3.5 - def dropWhile[A](l: List[A], f: A => Boolean): List[A]") {
    assert(List(4, 5) == List.dropWhile(List(1, 2, 3, 4, 5), (i: Int) => i < 4))
  }

  test("Exercise 3.6 - def init[A](l: List[A]): List[A]") {
    assert(List(1, 2, 3) == List.init(List(1, 2, 3, 4)))
    assert(List(1) == List.init(List(1, 2)))
    assert(List() == List.init(List(1)))
  }

  test("Exercise 3.8 - pass Nil and Cons to foldRight") {
    assert(List.foldRight(List(1,2,3), Nil: List[Int])(Cons(_, _)) == List(1, 2, 3))
    // Trace:
    // Cons(1, foldRight(List(2, 3), Nil)(Cons(_,_)))
    // Cons(1, Cons(2, foldRight(List(3), Nil)(Cons(_,_))))
    // Cons(1, Cons(2, Cons(3, Nil)))
  }

  test("Exercise 3.9 - def length[A](as: List[A]): Int") {
    assert(List.length(List(1, 2, 3)) == 3)
    assert(List.length(List("1", "2", "3", "4")) == 4)
  }

  test("Exercise 3.10 - def foldLeft(as: List[A], z: B)(f: (B, A) => B): B") {
    assert(List.foldLeft(List(1, 2, 3), 0)(_ + _) == 6)
    assert(List.foldLeft(List(1, 2, 3), 1)(_ * _) == 6)
    assert(List.foldLeft(List(1, 2, 3, 0), 1)(_ * _) == 0)
  }

  test("Exercise 3.11 - sum, product, length using foldLeft") {
    assert(List.sum3(List(1, 2, 3)) == 6)
    assert(List.product3(List(1.0, 2.0, 3.0)) == 6.0)
    assert(List.length3(List(1, 2, 3)) == 3)
  }

  test("Exercise 3.12 - def reverse[A](as: List[A]): List[A]") {
    assert(List.reverse(List(1,2,3)) == List(3,2,1))
  }

  test("Exercise 3.13 - foldLeft with foldRight") {
    assert(List.foldLeft2(List(1, 2, 3), 0)(_ + _) == 6)
    assert(List.foldLeft2(List(1, 2, 3, 4), 0)(_ + _) == 10)
    assert(List.foldLeft2(List(1, 2, 3), 1)(_ * _) == 6)
    assert(List.foldLeft2(List(1, 2, 3, 0), 1)(_ * _) == 0)
    assert(List.foldLeft2(List(1, 2, 3, 4), 1)(_ * _) == 24)
  }

  test("Exercise 3.13 - foldRight with foldLeft") {
    assert(List.foldLeft2(List(1, 2, 3), 0)(_ + _) == 6)
    assert(List.foldLeft2(List(1, 2, 3, 4), 0)(_ + _) == 10)
    assert(List.foldLeft2(List(1, 2, 3), 1)(_ * _) == 6)
    assert(List.foldLeft2(List(1, 2, 3, 0), 1)(_ * _) == 0)
    assert(List.foldLeft2(List(1, 2, 3, 4), 1)(_ * _) == 24)
  }

  test("Exercise 3.14 - def append[A](as: List[A], a:): List[A]") {
    assert(List.append1(List(1,2,3), List(4, 5)) == List(1,2,3,4,5))
    assert(List.append2(List(1,2,3), List(4, 5)) == List(1,2,3,4,5))
  }

  test("Exercise 3.15 - def concat[A](l: List[List[A]]): List[A]") {
    assert(List.concat1(List(List(1,2,3), List(4,5,6))) == List(1,2,3,4,5,6))
    assert(List.concat2(List(List(1,2,3), List(4,5,6))) == List(1,2,3,4,5,6))
  }

  test("Exercise 3.16 - def addOne(l: List[Int]): List[Int]") {
    assert(List.addOne(List(1,2,3)) == List(2,3,4))
    assert(List.addOne2(List(1,2,3)) == List(2,3,4))
  }

  test("Exercise 3.17 - def eachToString(l: List[Double]): List[String]") {
    assert(List.eachToString(List(1.0, 2.0, 3.0)) == List("1.0", "2.0", "3.0"))
  }

  test("Exercise 3.18 - def map[A,B](as: List[A])(f: A => B): List[B]") {
    assert(List.map(List(1.0, 2.0, 3.0))(_.toString) == List("1.0", "2.0", "3.0"))
  }

  test("Exercise 3.19 - def filter[A](as: List[A])(f: A => Boolean): List[A]") {
    assert(List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))
  }

  test("Exercise 3.20 - def flatMap[A,B](as: List[A])(f: A => List[B]): List[B]") {
    assert(List.flatMap(List(1,2,3))(i => List(i,i)) == List(1,1,2,2,3,3))
  }

  test("Exercise 3.21 - def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A]") {
    assert(List.filter(List(1,2,3,4,5,6))(_ % 2 == 0) == List(2,4,6))
  }

  test("Exercise 3.22 - def add(a: List[A], b: List[A]): List[A]") {
    assert(List.add(List(1,2,3), List(4,5,6)) == List(5,7,9))
    assert(List.add2(List(1,2,3), List(4,5,6)) == List(5,7,9))
  }

  test("Exercise 3.23 - def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A]") {
    assert(List.zipWith(List(1,2,3), List(4,5,6))(_+_) == List(5,7,9))
    assert(List.zipWith2(List(1,2,3), List(4,5,6))(_+_) == List(5,7,9))
  }

  test("Exercise 3.24 - def hasSubsequence(a: List[Int], b: List[Int]): Boolean") {
    assert(List.hasSubsequence(List(1,2,3), List(1,2)))
    assert(List.hasSubsequence(List(1,2,3), List(2,3)))
    assert(!List.hasSubsequence(List(1,2,3), List(1,1)))
    assert(!List.hasSubsequence(List(1,2,3), List(2,2)))
    assert(!List.hasSubsequence(List(1,2,3), List(3,3)))
    assert(!List.hasSubsequence(List(1,2,3), List(2,1)))
    assert(List.hasSubsequence(List(1,2,3), List(1)))
    assert(List.hasSubsequence(List(1,2,3), List(2)))
    assert(List.hasSubsequence(List(1,2,3), List(3)))
  }

  test("Exercise 3.25 - def size[A](left: Tree[A], right: Tree[A]): Int") {
    assert(Tree.size(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7)
    assert(Tree.size(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 9)
  }

  test("Exercise 3.26 - def maximum(t: Tree[Int]): Int") {
    assert(Tree.maximum(Branch(Leaf(1), Leaf(1))) == 1)
    assert(Tree.maximum(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 5)
    assert(Tree.maximum(Branch(Branch(Leaf(28), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 28)
    assert(Tree.maximum(Branch(Branch(Leaf(28), Leaf(2)), Branch(Leaf(33), Branch(Leaf(4), Leaf(5))))) == 33)
  }

  test("Exercise 3.27 - def depth[A](t: Tree[A]): Int") {
    assert(Tree.depth(Branch(Leaf(1), Leaf(1))) == 2)
    assert(Tree.depth(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 3)
    assert(Tree.depth(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) == 3)
    assert(Tree.depth(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1))) == 4)
  }

  test("Exercise 3.28 - def map[A,B](t: Tree[A])(f: A => B): Tree[B]") {
    assert(Tree.map(Branch(Leaf(1), Leaf(1)))(_ + 1) == Branch(Leaf(2), Leaf(2)))
    assert(Tree.map(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)))(_ + 1) == Branch(Branch(Branch(Leaf(2), Leaf(2)), Leaf(2)), Leaf(2)))
  }

  test("Exercise 3.29 - Rewrite Using Fold") {

    // Size
    assert(Tree.size2(Branch(Leaf(1), Leaf(2))) == 3)
    assert(Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Leaf(4)))) == 7)
    assert(Tree.size2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 9)

    // Maximum
    assert(Tree.maximum2(Branch(Leaf(1), Leaf(1))) == 1)
    assert(Tree.maximum2(Branch(Branch(Leaf(1), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 5)
    assert(Tree.maximum2(Branch(Branch(Leaf(28), Leaf(2)), Branch(Leaf(3), Branch(Leaf(4), Leaf(5))))) == 28)
    assert(Tree.maximum2(Branch(Branch(Leaf(28), Leaf(2)), Branch(Leaf(33), Branch(Leaf(4), Leaf(5))))) == 33)

    // Depth
    assert(Tree.depth2(Branch(Leaf(1), Leaf(1))) == 2)
    assert(Tree.depth2(Branch(Leaf(1), Branch(Leaf(1), Leaf(1)))) == 3)
    assert(Tree.depth2(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1))) == 3)
    assert(Tree.depth2(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1))) == 4)

    // Map
    assert(Tree.map2(Branch(Leaf(1), Leaf(1)))(_ + 1) == Branch(Leaf(2), Leaf(2)))
    assert(Tree.map2(Branch(Branch(Branch(Leaf(1), Leaf(1)), Leaf(1)), Leaf(1)))(_ + 1) == Branch(Branch(Branch(Leaf(2), Leaf(2)), Leaf(2)), Leaf(2)))
  }
}
