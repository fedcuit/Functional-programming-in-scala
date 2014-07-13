package io.github.fedcuit.fpinscala

object Recursion {
  def fib(n: Int): Int = {
    def go(a: Int, b: Int): Stream[Int] = a #:: go(b, a + b)
    go(0, 1).take(n).last
  }
}
