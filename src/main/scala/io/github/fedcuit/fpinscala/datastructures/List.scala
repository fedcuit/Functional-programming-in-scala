package io.github.fedcuit.fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](xs: List[A], init: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => init
      case Cons(h, t) => f(foldRight(t, init)(f), h)
    }
  }

  def foldLeft[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => acc
      case Cons(h, Nil) => f(acc, h)
      case Cons(h, t) => foldLeft(init(xs), f(acc, last(xs)))(f)
    }
  }

  def sum(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)

  def sum2(xs: List[Int]): Int = foldLeft(xs, 0)(_ + _)

  def product(xs: List[Double]): Double = foldRight(xs, 1d)(_ * _)

  def product2(xs: List[Double]): Double = foldLeft(xs, 1d)(_ * _)

  def apply[A](as: A*): List[A] =
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  def tail[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => throw new RuntimeException("empty list don't have a tail")
      case Cons(h, Nil) => Nil
      case _ => drop(xs, 1)
    }
  }

  def drop[A](xs: List[A], n: Int): List[A] = {
    if (n == 0) {
      xs
    } else
      xs match {
        case Nil => Nil
        case Cons(h, Nil) => Nil
        case Cons(h, t) => drop(t, n - 1)
      }
  }

  def dropWhile[A](xs: List[A])(f: A => Boolean): List[A] = {
    xs match {
      case Nil => Nil
      case Cons(h, t) => if (f(h)) dropWhile(t)(f) else xs
    }
  }

  def setHead[A](xs: List[A], newHead: A): List[A] = {
    xs match {
      case Nil => throw new RuntimeException("can no set head for a empty list")
      case Cons(h, t) => Cons(newHead, t)
    }
  }

  def init[A](xs: List[A]): List[A] = {
    xs match {
      case Nil => throw new RuntimeException("empty list don't have a tail")
      case Cons(h, Nil) => Nil
      case Cons(h, t) => Cons(h, init(t))
    }
  }

  def last[A](xs: List[A]): A = {
    xs match {
      case Nil => throw new RuntimeException("can not call last on empty list")
      case Cons(last, Nil) => last
      case Cons(h, t) => last(t)
    }
  }

  def append[A](xs: List[A], ys: List[A]): List[A] = {
    xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, append(t, ys))
    }
  }

  def append2[A](xs: List[A], ys: List[A]): List[A] = {
    foldRight(xs, ys)((acc, x) => Cons(x, acc))
  }

  def reverse[A](xs: List[A]): List[A] = foldRight(xs, List[A]())((ys, x) => append(ys, List(x)))

  def length[A](xs: List[A]): Int = foldRight(xs, 0)((ys, x) => ys + 1)

  def length2[A](xs: List[A]): Int = foldLeft(xs, 0)((ys, x) => ys + 1)
}
