package com.billtheconqueror.chapterthree

// The beginning of this I got from the official fpinscala GitHub Repo,
// and I have filled in the exercises on my own.
sealed trait List[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends List[Nothing]
case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {
  def sum(ints: List[Int]): Int = ints match {
    case Nil => 0
    case Cons(x, xs) => x + sum(xs)
  }

  def product(ds: List[Double]): Double = ds match {
    case Nil => 1.0
    case Cons(0.0, _) => 0.0
    case Cons(x, xs) => x * product(xs)
  }

  def apply[A](as: A*): List[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def foldRight[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    as match {
      case Nil => z
      case Cons(x, xs) => f(x, foldRight(xs, z)(f))
    }
  }

  /** Exercise 3.1 */
  val x = List(1, 2, 3, 4, 5) match {
    case Cons(x, Cons(2, Cons(4, _))) => x
    case Nil => 42
    case Cons(x, Cons(y, Cons(3, Cons(4, _)))) => x + y
    case Cons(h, t) => h + sum(t)
    case _ => 101
  }

  /** Exercise 3.2 */
  def tail[A](l: List[A]): List[A] = {
    l match {
      // Need to do something other than return Nil here, but still need to learn that in the coming chapters
      case Nil => Nil
      case Cons(_, t) => t
    }
  }

  /** Exercise 3.3 */
  def setHead[A](l: List[A], nh: A): List[A] = {
    l match {
      case Nil => List(nh)
      case Cons(_, t) => Cons(nh, t)
    }
  }

  /** Exercise 3.4 */
  @annotation.tailrec
  def drop[A](l: List[A], n: Int): List[A] = {
    if (n ==  0) return l

    l match {
      // Need to do something other than return Nil here, but still need to learn that in the coming chapters
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
  }

  /** Exercise 3.5 */
  @annotation.tailrec
  def dropWhile[A](l: List[A], f: A => Boolean): List[A] = {
    l match {
      // Need to do something other than return Nil here, but still need to learn that in the coming chapters
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t, f) else l
    }
  }

  /** Exercise 3.6 */
  def init[A](l: List[A]): List[A] = {
    // Non tail recursive solution
    /*
    l match {
      // Need to do something other than return Nil here, but still need to learn that in the coming chapters
      case Nil => Nil
      case Cons(_, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
    */

    // Drop the last element
    // In order to maintain Tail Recursion here I could
    // not figure out how to not reverse the order of the elements.
    // The second loop below rearranges the elements back to their
    // original order.
    @annotation.tailrec
    def loop1(ls: List[A], lf: List[A]): List[A] = {
      ls match {
        case Cons(_, Nil) => lf
        case Cons(h, t) => loop1(t, Cons(h, lf))
      }
    }

    // Rearrange the elements in original order
    @annotation.tailrec
    def loop2(ls: List[A], lf: List[A]): List[A] = {
      ls match {
        case Nil => lf
        case Cons(h, Nil) => Cons(h, lf)
        case Cons(h, t) => loop2(t, Cons(h, lf))
      }
    }
    loop2(loop1(l, List()), List())
  }

  /** Exercise 3.7 */
  // Recursion can halt immediately, but 0.0 will still be multiplied
  // by each value that has already been seen.  For example, when 0.0
  // is hit in (1.0, 2.0, 3.0, 0.0, 4.0), the resulting stack would
  // look like: (1.0 * (2.0 * (3.0 * (0.0)))).  0.0 would have to be
  // multiplied by 3.0, then 2.0, then 1.0 still.

  /** Exercise 3.8 */
  // See unit tests

  /** Exercise 3.9 */
  def length[A](as: List[A]): Int = {
    foldRight(as, 0)((_, y) => y + 1)
  }

  /** Exercise 3.10 */
  def foldLeft[A, B](as: List[A], z: B)(f: (B, A) => B): B = {

    @annotation.tailrec
    def loop(l: List[A], acc: B): B = {
      l match {
        case Nil => z
        case Cons(h, Nil) => f(acc, h)
        case Cons(h, t) => loop(t, f(acc, h))
      }
    }

    loop(as, z)
  }

  /** Exercise 3.11 */
  def sum3(as: List[Int]): Int = {
    foldLeft(as, 0)(_ + _)
  }
  def product3(as: List[Double]): Double = {
    foldLeft(as, 1.0)(_ * _)
  }
  def length3[A](as: List[A]): Int = {
    foldLeft(as, 0)((x,_) => x + 1)
  }

  /** Exercise 3.12 */
  def reverse[A](as: List[A]): List[A] = {
    foldLeft(as, List[A]())((x, y) => Cons(y, x))
  }

  /** Exercise 3.13 */
  /** NOTE: I had to get help on this one from the answer key */
  def foldLeft2[A,B](as: List[A], z: B)(f: (B, A) => B): B = {
    foldRight(as, (b:B) => b)((a,g) => b => g(f(b,a)))(z)
  }

  def foldRight2[A,B](as: List[A], z: B)(f: (A, B) => B): B = {
    foldLeft(as, (b:B) => b)((g,a) => b => g(f(a,b)))(z)
  }

  /** Exercise 3.14 */
  def append1[A](l: List[A], r: List[A]): List[A] = {
    foldLeft(reverse(l), r)((a, b) => Cons(b, a))
  }
  def append2[A](l: List[A], r: List[A]): List[A] = {
    foldRight(l, r)((a, b) => Cons(a, b))
  }

  /** Exercise 3.15 */
  def concat1[A](l: List[List[A]]): List[A] = {
    // Although this works, it does not meet the time constraints of linear time O(n) because
    // the first argument in append2 grows as each list is appended.
    foldLeft(l, List[A]())(append2(_, _))
  }

  def concat2[A](l: List[List[A]]): List[A] = {
    // Although this works, it does not meet the time constraints of linear time O(n) because
    // the first argument in append2 grows as each list is appended.
    foldRight(l, List[A]())(append2(_, _))
  }

  /** Exercise 3.16 */
  def addOne(l: List[Int]): List[Int] = {

    @annotation.tailrec
    def loop(x: List[Int], acc: List[Int]): List[Int] = {
      x match {
        case Nil => Nil
        case Cons(h, Nil) => Cons(h+1, acc)
        case Cons(h, t) => loop(t, Cons(h+1, acc))
      }
    }

    reverse(loop(l, List[Int]()))
  }

  // Another implementation using foldRight/foldLeft
  def addOne2(l: List[Int]): List[Int] = {
//    foldRight(l, List[Int]())((a,b) => Cons(a+1, b))
//    reverse(foldLeft(l, List[Int]())((b,a) => Cons(a+1, b)))
    foldRight2(l, List[Int]())((a,b) => Cons(a+1, b))
  }

  /** Exercise 3.17 */
  def eachToString(l: List[Double]): List[String] = {
//    foldRight(l, List[String]())((a,b) => Cons(a.toString, b))
//    reverse(foldLeft(l, List[String]())((b,a) => Cons(a.toString, b)))
    foldRight2(l, List[String]())((a,b) => Cons(a.toString, b))
  }

  /** Exercise 3.18 */
  def map[A,B](as: List[A])(f: A => B): List[B] = {
//    reverse(foldLeft(as, List[B]())((b,a) => Cons(f(a), b)))
    foldRight2(as, List[B]())((a,b) => Cons(f(a), b))
  }

  /** Exercise 3.19 */
  def filter[A](as: List[A])(f: A => Boolean): List[A] = {
    foldRight2(as, List[A]())((a,b) => if(f(a)) Cons(a, b) else b)
  }

  /** Exercise 3.20 */
  def flatMap[A,B](as: List[A])(f: A => List[B]): List[B] = {
    foldRight2(as, List[B]())((a,b) => append2(f(a), b))
  }

  /** Exercise 3.21 */
  def filterWithFlatMap[A](as: List[A])(f: A => Boolean): List[A] = {
    flatMap(as)(a => if(f(a)) List(a) else List[A]())
  }

  /** Exercise 3.22 */
  /** My solution without looking at the answer */
  def add(a: List[Int], b: List[Int]): List[Int] = {

    @annotation.tailrec
    def loop(c: List[Int], d: List[Int], e: List[Int]): List[Int] = {
      val (xh,xt): (Int,List[Int]) = c match {
        case Cons(h, t) => (h, t)
      }
      val (yh,yt): (Int,List[Int]) = d match {
        case Cons(h, t) => (h, t)
      }

      if (xt == Nil || yt == Nil) return List.append2[Int](e, List[Int](xh+yh))
      loop(xt, yt, List.append2[Int](e, List[Int](xh+yh)))
    }

    loop(a, b, List[Int]())
  }

  /** Exercise 3.22 */
  /** My solution after looking at the answer */
  def add2(a: List[Int], b: List[Int]): List[Int] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(xh+yh, add2(xt, yt))
  }

  /** Exercise 3.23 */
  /** My solution without looking at the answer */
  def zipWith[A](a: List[A], b: List[A])(f: (A,A) => A): List[A] = {

    @annotation.tailrec
    def loop(c: List[A], d: List[A], e: List[A]): List[A] = {
      val (xh,xt): (A,List[A]) = c match {
        case Cons(h, t) => (h, t)
      }
      val (yh,yt): (A,List[A]) = d match {
        case Cons(h, t) => (h, t)
      }

      if (xt == Nil || yt == Nil) return List.append2[A](e, List[A](f(xh,yh)))
      loop(xt, yt, List.append2[A](e, List[A](f(xh,yh))))
    }

    loop(a, b, List[A]())
  }

  /** Exercise 3.23 */
  /** My solution after looking at the answer */
  def zipWith2[A,B,C](a: List[A], b: List[B])(f: (A,B) => C): List[C] = (a,b) match {
    case (Nil, _) => Nil
    case (_, Nil) => Nil
    case (Cons(xh, xt), Cons(yh, yt)) => Cons(f(xh,yh), zipWith2(xt, yt)(f))
  }

  /** Exercise 3.24 */
  def hasSubsequence(a: List[Int], b: List[Int]): Boolean = {

    @annotation.tailrec
    def loop(as: List[Int], bs: List[Int]): Boolean = {
      (as, bs) match {
        case (Cons(ah, Nil), Cons(bh, Nil)) => ah == bh
        case (Cons(_, Nil), Cons(_, _)) => false
        case (Cons(ah, at), Cons(bh, Nil)) => if (ah == bh) true else loop(at, bs)
        case (Cons(ah, at), Cons(bh, bt)) => if (ah == bh) loop(at, bt) else loop(at, bs)
      }
    }

    loop(a, b)
  }
}

