package io.github.fedcuit.fpinscala.laziness

import org.scalatest.{FunSpec, Matchers}

class Stream$Test extends FunSpec with Matchers {
  describe("Stream") {
    describe("toList") {
      it("should able to convert a stream into a list") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.toList(stream) should be(List(1, 2, 3, 4, 5))
      }
    }

    describe("take") {
      it("should take the first n elements") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.toList(Stream.take(stream, 3)) should be(List(1, 2, 3))
      }
    }

    describe("takeWhile") {
      it("should take the all starting elements which match the given predicate") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.toList(Stream.takeWhile(stream)(_ < 4)) should be(List(1, 2, 3))
      }
    }

    describe("foldRight") {
      it("should apply transformation to each element and accumulate them") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.foldRight(stream, 0)(_ + _) should be(15)
      }
    }

    describe("exists") {
      it("should return true if any element match the predicate") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.exists(stream)(_ == 4) should be (true)
      }
    }

    describe("exists2") {
      it("should has the same functionality with `exists` and it's short-circuit") {
        // the early termination is enabled because `or` operator, not-strict parameter only get evaluated when it's referenced,
        // so when `true || predicate('1')`, the right side will never be executed and recursion will be ended.
        val stream = Stream(1, 2, 3, 4, 5)
        // check the output in console
        Stream.exists2(stream)(_ == 4) should be (true)
      }
    }

    describe("exists3") {
      it("should has the same functionality with `exists2`") {
        val stream = Stream(1, 2, 3, 4, 5)
        Stream.exists3(stream)(_ == 4) should be (true)
      }
    }
  }
}
