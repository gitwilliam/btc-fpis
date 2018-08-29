package com.billtheconqueror.exercises

object ChapterTwo {

  /** Exercise 2.1 */
  def fib(n: Int): Int = {
    if (n == 0) return 0
    if (n == 1) return 1

    @annotation.tailrec
    def loop(i: Int, a: Int, b: Int): Int = {
      if (i == n) a+b
      else loop(i+1, b, a+b)
    }

    loop(2, 0, 1)
  }

  /** Exercise 2.2 */
  def isSorted[A](as: Array[A], ordered: (A,A) => Boolean): Boolean = {
    if (as.length == 0) return false
    if (as.length == 1) return true

    @annotation.tailrec
    def loop(n: Int): Boolean = {
      if (as.length == n+1) true
      else if (ordered(as(n), as(n+1))) loop(n+1)
      else false
    }

    loop(0)
  }

  /** Exercise 2.3 */
  def curry[A,B,C](f: (A,B) => C): A => (B => C) = {
    a => b => f(a, b)
  }

  /** Exercise 2.4 */
  def uncurry[A,B,C](f: A => B => C): (A, B) => C = {
    (a, b) => f(a)(b)
  }

  /** Exercise 2.5 */
  def compose[A,B,C](f: B => C, g: A => B): A => C = {
    a => f(g(a))
  }
}