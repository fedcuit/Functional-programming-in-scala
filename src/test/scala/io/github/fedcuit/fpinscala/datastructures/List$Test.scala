package io.github.fedcuit.fpinscala.datastructures

import org.scalatest.{FunSpec, Matchers}

class List$Test extends FunSpec with Matchers {
  describe("List") {
    describe("tail method") {
      it("should throw exception when extract tail of a empty list") {
        intercept[RuntimeException] {
          List.tail(Nil)
        }
      }

      it("should return Nil when extract tail of a list which only contains one element") {
        List.tail(Cons("i'm head", Nil)) should be(Nil)
      }

      it("should return all the elements except the first one when extract tail of a list") {
        List.tail(Cons("i'm head", Cons("i'm second", Cons("i'm third", Nil)))) should be(Cons("i'm second", Cons("i'm third", Nil)))
      }
    }

    describe("drop method") {
      it("should return Nil when drop the first 2 element of a empty list") {
        List.drop(Nil, 2) should be(Nil)
      }

      it("should return Nil when drop the first 2 element of a list which only contains one element") {
        List.drop(Cons("only one", Nil), 2) should be(Nil)
      }

      it("should return the last element when drop the first 2 element aof a list which contains 3 elements") {
        List.drop(Cons("i'm head", Cons("i'm second", Cons("i'm third", Nil))), 2) should be(Cons("i'm third", Nil))
      }
    }

    describe("dropWhile method") {
      it("should return Nil when drop while element bigger that 1 of a empty list") {
        List.dropWhile[Int](Nil)(_ > 1) should be(Nil)
      }

      it("should return the first continuous n elements when drop while element bigger that 1 of a empty list") {
        List.dropWhile(Cons(2, Cons(-3, Cons(-7, Cons(5, Nil)))))(_ > 1) should be(Cons(-3, Cons(-7, Cons(5, Nil))))
      }

    }

    describe("setHead method") {
      it("should throw exception when setHead for a empty list") {
        intercept[RuntimeException] {
          List.setHead(Nil, "head")
        }
      }

      it("should return a list with new head for call setHead") {
        List.setHead(Cons("oldHead", Nil), "newHead") should be(Cons("newHead", Nil))
      }
    }

  }
}
