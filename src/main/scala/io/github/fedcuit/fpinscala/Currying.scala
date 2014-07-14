package io.github.fedcuit.fpinscala

object Currying {
  def times(n: Int)(f: => Unit) {
    (1 to n).foreach(_ => f)
  }

  def main(args: Array[String]) {
    times(2) {
      println("The Flash")
    }
  }
}
