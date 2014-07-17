package io.github.fedcuit.fpinscala.datastructures

sealed trait List[+A]

case object Nil extends List[Nothing]

case class Cons[+A](head: A, tail: List[A]) extends List[A]

object List {

  def foldRight[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => acc
      case Cons(h, t) => f(foldRight(t, acc)(f), h)
    }
  }

  def foldRight2[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = {
    xs match {
      case Nil => acc
      case Cons(h, Nil) => f(acc, h)
      case Cons(h, t) => foldRight2(init(xs), f(acc, last(xs)))(f)
    }
  }

  def foldLeft[A, B](xs: List[A], acc: B)(f: (B, A) => B): B = {
    xs match {
      case Cons(h, Nil) => f(acc, h)
      case Cons(h, t) => foldLeft(t, f(acc, h))(f)
    }
  }

  def sum(xs: List[Int]): Int = foldRight(xs, 0)(_ + _)

  def sum2(xs: List[Int]): Int = foldRight2(xs, 0)(_ + _)

  def product(xs: List[Double]): Double = foldRight(xs, 1d)(_ * _)

  def product2(xs: List[Double]): Double = foldRight2(xs, 1d)(_ * _)

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

  def head[A](xs: List[A]): A = {
    xs match {
      case Nil => throw new RuntimeException("empty list doesn't have a head")
      case Cons(h, t) => h
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

  def length2[A](xs: List[A]): Int = foldRight2(xs, 0)((ys, x) => ys + 1)

  def concat[A](ls: List[A]*): List[A] = {
    if (ls.isEmpty) Nil
    else if (ls.size == 2) append(ls.head, ls.last)
    else append(ls.head, concat(ls.tail: _*))
  }

  def map[A, B](xs: List[A])(f: A => B): List[B] = {
    foldRight2(xs, List[B]())((ys, x) => Cons(f(x), ys))
  }

  def filter[A](xs: List[A])(f: A => Boolean): List[A] = {
    foldRight(xs, List[A]())((ys, x) => if (f(x)) Cons(x, ys) else ys)
  }

  def filter2[A](xs: List[A])(f: A => Boolean): List[A] = {
    flatMap(xs)(x => if (f(x)) List(x) else Nil)
  }

  def flatMap[A, B](xs: List[A])(f: A => List[B]): List[B] = foldLeft(xs, List[B]())((ys, x) => append(ys, f(x)))

  def zipTo[A, B, E](xs: List[A], ys: List[B])(f: (A, B) => E): List[E] = {
    xs match {
      case Nil => Nil
      case Cons(h, t) => Cons(f(h, head(ys)), zipTo(t, tail(ys))(f))
    }
  }

  def take[A](xs: List[A], n: Int): List[A] = {
    def takeF(xs: List[A], temp: List[A], n: Int): List[A] =
      if (length(xs) < n) xs
      else if (n == 0) temp
      else reverse(takeF(tail(xs), Cons(head(xs), temp), n - 1))

    takeF(xs, List[A](), n)
  }

  def takeWhile[A](xs: List[A])(f: A => Boolean): List[A] = {
    def takeF(xs: List[A], temp: List[A]): List[A] = {
      if (!f(head(xs))) temp
      else reverse(takeF(tail(xs), Cons(head(xs), temp)))
    }


    takeF(xs, List[A]())
  }

  def forAll[A](xs: List[A])(f: A => Boolean): Boolean = {
    foldLeft(xs, true)((ys, x) => ys && f(x))
  }

  def exists[A](xs: List[A])(f: A => Boolean): Boolean = {
    foldLeft(xs, false)((ys, x) => ys || f(x))
  }

  def hasSubSequence[A](xs: List[A], ys: List[A]): Boolean = {
    val sub = dropWhile(xs)(_ == head(ys))
    if (sub == Nil) false
    else if (length(ys) == 1 && sub != null) true
    else hasSubSequence(sub, tail(ys))
  }
}
