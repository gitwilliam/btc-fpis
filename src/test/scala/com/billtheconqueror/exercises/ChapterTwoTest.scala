package com.billtheconqueror.exercises
import org.scalatest.FunSuite

class ChapterTwoTest extends FunSuite {

  /** Tests for Exercise 2.1 */
  test("Exercise 2.1 - def fib(n: Int): Int") {
    assert(ChapterTwo.fib(0) == 0)
    assert(ChapterTwo.fib(1) == 1)
    assert(ChapterTwo.fib(2) == 1)
    assert(ChapterTwo.fib(3) == 2)
    assert(ChapterTwo.fib(4) == 3)
    assert(ChapterTwo.fib(5) == 5)
    assert(ChapterTwo.fib(30) == 832040)
  }

  test("Exercise 2.2 - def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean") {
    assert(!ChapterTwo.isSorted(Array[Int](), (a: Int, b: Int) => a < b))
    assert(ChapterTwo.isSorted(Array(1), (a: Int, b: Int) => a < b))
    assert(ChapterTwo.isSorted(Array(1, 2, 3), (a: Int, b: Int) => a < b))
    assert(ChapterTwo.isSorted(Array(10, 12, 14, 16), (a: Int, b: Int) => a < b))
    assert(!ChapterTwo.isSorted(Array(100, 2, 3), (a: Int, b: Int) => a < b))
    assert(!ChapterTwo.isSorted(Array(100, 200, 3), (a: Int, b: Int) => a < b))
    assert(!ChapterTwo.isSorted(Array(3, 2, 1), (a: Int, b: Int) => a < b))
    assert(ChapterTwo.isSorted(Array('A', 'B', 'C'), (a: Char, b: Char) => a < b))
    assert(!ChapterTwo.isSorted(Array('C', 'B', 'A'), (a: Char, b: Char) => a < b))
  }

  test("Exercise 2.3 - def curry[A,B,C](f: (A,B) => C): A => (B => C)") {
    assert(ChapterTwo.curry[Int, String, String]((a: Int, b: String) => a.toString().concat(b))(2)(" = Two") == "2 = Two")
  }

  test("Exercise 2.4 - def uncurry[A,B,C](f: A => B => C): (A, B) => C") {
    assert(ChapterTwo.uncurry[Int, Int, Int](a => b => a + b)(2, 3) == 5)
  }

  test("Exercise 2.5 - def compose[A,B,C](f: B => C, g: A => B): A => C") {
    assert(ChapterTwo.compose[Int, Int, Int](b => b+1, a => a+100)(0) == 101)
  }

}
