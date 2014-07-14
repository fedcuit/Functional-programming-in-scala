package io.github.fedcuit.fpinscala.datastructures

sealed trait List[+A]

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
}
