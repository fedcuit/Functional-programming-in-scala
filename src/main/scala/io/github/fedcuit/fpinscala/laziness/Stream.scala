package io.github.fedcuit.fpinscala.laziness

trait Stream[+A] {
  // retrieve a element from stream
  def uncons: Option[(A, Stream[A])]

  def isEmpty: Boolean = uncons.isEmpty
}

object Stream {
  // generate a empty stream
  def empty[A]: Stream[A] = {
    new Stream[A] {
      override def uncons: Option[(A, Stream[A])] = None
    }
  }

  def cons[A](head: => A, tail: => Stream[A]): Stream[A] = {
    new Stream[A] {
      lazy val uncons = Some((head, tail))
    }
  }

  def apply[A](as: A*): Stream[A] = {
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))
  }

  def toList[A](s: Stream[A]): List[A] = {
    if (s.isEmpty) Nil
    else s.uncons.get._1 :: toList(s.uncons.get._2)
  }

  def take[A](s: Stream[A], n: Int): Stream[A] = {
    if (n == 0) empty
    else cons(s.uncons.get._1, take(s.uncons.get._2, n - 1))
  }

  def takeWhile[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
    if (!f(s.uncons.get._1)) empty
    else cons(s.uncons.get._1, takeWhile(s.uncons.get._2)(f))
  }

  def foldRight[A, B](s: Stream[A], acc: B)(f: (A, B) => B): B = {
    if (s.isEmpty) acc
    else foldRight(s.uncons.get._2, f(s.uncons.get._1, acc))(f)
  }

  def foldRight2[A, B](s: Stream[A], acc: => B)(f: (A, => B) => B): B = {
    if (s.isEmpty) acc
    else foldRight2(s.uncons.get._2, f(s.uncons.get._1, acc))(f)
  }

  def foldRight3[A, B](s: Stream[A], acc: => B)(f: (A, => B) => B): B = {
    if (s.isEmpty) acc
    else f(s.uncons.get._1, foldRight3(s.uncons.get._2, acc)(f))
  }

  def exists[A](s: Stream[A])(f: A => Boolean): Boolean = {
    foldRight(s, false)((x, ys) => {
      println(s"check $x")
      f(x) || ys
    })
  }

  def exists2[A](s: Stream[A])(f: A => Boolean): Boolean = {
    foldRight2(s, false)((x, ys) => {
      println(s"check $x")
      f(x) || ys
    })
  }

  def exists3[A](s: Stream[A])(f: A => Boolean): Boolean = {
    foldRight3(s, false)((x, ys) => {
      println(s"check $x")
      f(x) || ys
    })
  }

  def forAll[A](s: Stream[A])(f: A => Boolean): Boolean = {
    foldRight3(s, true)((x, ys) => {
      println(s"check $x")
      f(x) && ys
    })
  }

  def map[A, B](s: Stream[A])(f: A => B): Stream[B] = {
    foldRight3(s, empty[B])((x, ys) => {
      println(s"map $x to ${f(x)}")
      cons(f(x), ys)
    })
  }

  def filter[A](s: Stream[A])(f: A => Boolean): Stream[A] = {
    foldRight3(s, empty[A])((x, ys) => {
      println(s"select $x")
      if (f(x)) cons(x, ys) else ys
    })
  }
}
