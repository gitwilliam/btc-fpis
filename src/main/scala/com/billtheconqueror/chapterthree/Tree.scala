package com.billtheconqueror.chapterthree

sealed trait Tree[+A]
case class Leaf[A](value: A) extends Tree[A]
case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  /** Exercise 3.25 */
  def size[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + size(l) + size(r)
    }
  }

  /** Exercise 3.26 */
  def maximum(t: Tree[Int]): Int = {

    def loop(ts: Tree[Int], m: Int): Int = {
      ts match {
        case Leaf(v) => v.max(m)
        case Branch(l,r) => loop(l, m).max(loop(r, m))
      }
    }

    loop(t, Int.MinValue)
  }

  /** Exercise 3.27 */
  def depth[A](t: Tree[A]): Int = {
    t match {
      case Leaf(_) => 1
      case Branch(l,r) => 1 + depth(l).max(depth(r))
    }
  }

  /** Exercise 3.28 */
  def map[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    t match {
      case Leaf(v) => Leaf(f(v))
      case Branch(l,r) => Branch(map(l)(f), map(r)(f))
    }
  }

  /** Exercise 3.29 */
  // I look at the hint on this one
  def fold[A,B](t: Tree[A])(l: A => B)(b: (B,B) => B): B = {
    t match {
      case Leaf(v) => l(v)
      case Branch(x,y) => b(fold(x)(l)(b),fold(y)(l)(b))
    }
  }

  def size2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((l,r) => 1 + l + r)
  }

  def maximum2(t: Tree[Int]): Int = {
    fold(t)(a => a)((a,b) => a.max(b))
  }

  def depth2[A](t: Tree[A]): Int = {
    fold(t)(_ => 1)((a,b) => 1 + a.max(b))
  }

  def map2[A,B](t: Tree[A])(f: A => B): Tree[B] = {
    fold[A, Tree[B]](t)(a => Leaf(f(a)))((a,b) => Branch(a,b))
  }
}

