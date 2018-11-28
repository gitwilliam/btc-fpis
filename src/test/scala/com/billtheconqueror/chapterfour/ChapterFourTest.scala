package com.billtheconqueror.chapterfour

import org.scalatest.FunSuite
import org.scalatest.Matchers._

class ChapterFourTest extends FunSuite {

  test("def map[B](f: A => B): Option[B]") {
    assert(None.map(a => a) == None)
    assert(Some(1).map(a => a + 1) == Some(2))
  }

  test("def flatMap[B](f: A => Option[B]): Option[B]") {
    assert(None.flatMap(a => a) == None)
    assert(Some(1).flatMap(_ => None) == None)
    assert(Some(1).flatMap(a => Some(a + 1)) == Some(2))
  }

  test("def getOrElse[B >: A](default: => B): B") {
    assert(None.getOrElse(1) == 1)
    assert(Some(1).getOrElse(1) == 1)
    assert(Some(2).getOrElse(1) == 2)
  }

  test("def orElse[B >: A](ob: => Option[B]): Option[B]") {
    assert(None.orElse(Some(1)) == Some(1))
    assert(Some(2).orElse(Some(1)) == Some(2))
  }

  test("def filter(f: A => Boolean): Option[A]") {
    assert(Some(1).filter(a => a < 1) == None)
    assert(Some(0).filter(a => a < 1) == Some(0))
  }

  test("def mean(xs: Seq[Double]): Option[Double]") {
    assert(Option.mean(Seq(1,2,3)) == Some(2.0))
    assert(Option.mean(Seq(1,2,3,4,5,6)) == Some(3.5))
  }

  test("def variance(xs: Seq[Double]): Option[Double]") {
    assert(Option.variance(Seq()) == None)
    assert(Option.variance(Seq(1,2,3,4,5,6)).getOrElse(0.0) === Some(2.917).getOrElse(1.0) +- 0.001)
  }

  test("def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C]") {
    assert(Option.map2(Some(1), None)((a: Int, b: Int) => a + b) == None)
    assert(Option.map2(None, Some(1))((a: Int, b: Int) => a + b) == None)
    assert(Option.map2(Some(1), Some(1))((a: Int, b: Int) => a + b) == Some(2))
  }

  test("def sequence[A](a: List[Option[A]]): Option[List[A]]") {
    assert(Option.sequence(List(Some(1), Some(2), None)) == None)
    assert(Option.sequence(List(None, Some(1), Some(2))) == None)
    assert(Option.sequence(List(Some(1), None, Some(2))) == None)
    assert(Option.sequence(List(Some(1), Some(2), Some(3))) == Some(List(1, 2, 3)))
  }
}
