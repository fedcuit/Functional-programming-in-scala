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

  def sum(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)

  def product(xs: List[Double]): Double = foldRight(xs, 1d)(_ * _)

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

  def append[A](xs: List[A], ys: List[A]): List[A] = {
    xs match {
      case Nil => ys
      case Cons(h, t) => Cons(h, append(t, ys))
    }
  }

  def reverse[A](xs: List[A]): List[A] = {
    foldRight(xs, List[A]())((ys: List[A], x: A) => List.append(ys, List(x)))
  }
}
