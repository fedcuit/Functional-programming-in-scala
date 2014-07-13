package io.github.fedcuit.fpinscala

object Generify {
  def isSorted[A](xs: Array[A])(f: (A, A) => Boolean): Boolean = {
    if (xs.size == 1) true
    else if (!f(xs.head, xs.tail.head)) false
    else isSorted(xs.tail)(f)
  }
}
