package com.billtheconqueror.chapterfour

sealed trait Option[+A] {

  // Exercise 4.1
  def map[B](f: A => B): Option[B] = {
    this match {
      case None => None
      case Some(a) => Some(f(a))
    }
  }

  def flatMap[B](f: A => Option[B]): Option[B] = {
    this match {
      case None => None
      case Some(a) => f(a)
    }
  }

  def getOrElse[B >: A](default: => B): B = {
    this match {
      case None => default
      case Some(a) => a
    }
  }

  def orElse[B >: A](ob: Option[B]): Option[B] = {
    if (this != None) this else ob
  }

  def filter(f: A => Boolean): Option[A] = {
    this match {
      case None => None
      case Some(a) => if (f(a)) this else None
    }
  }
}
case class Some[+A](get: A) extends Option[A]
case object None extends Option[Nothing]

object Option {
  // Exercise 4.2
  // Glanced at the answer for help on this one...
  def variance(xs: Seq[Double]): Option[Double] = {
    mean(xs).flatMap(mu => mean(xs.map(x => math.pow(x - mu, 2))))
  }

  def mean(xs: Seq[Double]): Option[Double] = {
    if (xs.isEmpty) None
    else Some(xs.foldRight(0.0)((x,y) => x + y)/xs.size)
  }



  // Exercise 4.3
  def map2[A,B,C](a: Option[A], b: Option[B])(f: (A, B) => C): Option[C] =  {
    if (a == None || b == None) None
    else a.flatMap(aa => b.map(bb => f(aa,bb)))
  }

  // Exercise 4.4
  def sequence[A](a: List[Option[A]]): Option[List[A]] = {
    //Some(a.map(_.getOrElse(return None)))
    traverse(a)(i => i.orElse(None))
  }

  // Exercise 4.5
  def traverse[A, B](a: List[A])(f: A => Option[B]): Option[List[B]] = {
    // sequence(a.map(i => f(i)))
    Some(a.map(i => f(i).getOrElse(return None)))
  }

  def Try[A](a: => A): Option[A] = {
    try Some(a)
    catch { case _: Exception => None}
  }
}
