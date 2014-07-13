package io.github.fedcuit.fpinscala

import org.scalatest.{FunSpec, Matchers}

class Recursion$Test extends FunSpec with Matchers {
  describe("Recursion") {
    it("should able to figure out the 6th fibonacci number") {
      Recursion.fib(6) should be(5)
    }
  }
}
