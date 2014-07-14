package io.github.fedcuit.fpinscala.datastructures

import org.scalatest.{FunSpec, Matchers}

class List$Test extends FunSpec with Matchers {
  describe("List") {
    describe("tail method") {
      it("should throw exception when extract tail of a empty list") {
        intercept[RuntimeException] {
          List.tail(List())
        }
      }

      it("should return Nil when extract tail of a list which only contains one element") {
        List.tail(List("i'm head")) should be(Nil)
      }

      it("should return all the elements except the first one when extract tail of a list") {
        List.tail(List("i'm head", "i'm second", "i'm third")) should be(List("i'm second", "i'm third"))
      }
    }

    describe("drop method") {
      it("should return Nil when drop the first 2 element of a empty list") {
        List.drop(List(), 2) should be(Nil)
      }

      it("should return Nil when drop the first 2 element of a list which only contains one element") {
        List.drop(List("only one"), 2) should be(Nil)
      }

      it("should return the last element when drop the first 2 element aof a list which contains 3 elements") {
        List.drop(List("i'm head", "i'm second", "i'm third"), 2) should be(List("i'm third"))
      }
    }

    describe("dropWhile method") {
      it("should return Nil when drop while element bigger that 1 of a empty list") {
        List.dropWhile[Int](Nil)(_ > 1) should be(Nil)
      }

      it("should return the first continuous n elements when drop while element bigger that 1 of a empty list") {
        List.dropWhile(List(2, -3, -7, 5))(_ > 1) should be(List(-3, -7, 5))
      }

    }

    describe("setHead method") {
      it("should throw exception when setHead for a empty list") {
        intercept[RuntimeException] {
          List.setHead(Nil, "head")
        }
      }

      it("should return a list with new head for call setHead") {
        List.setHead(List("oldHead"), "newHead") should be(List("newHead"))
      }
    }

    describe("init method") {
      it("should throw exception when operate a empty list") {
        intercept[RuntimeException] {
          List.init(List())
        }
      }

      it("should return Nil when operate a list which only have one element") {
        List.init(List(2)) should be(Nil)
      }

      it("should return all elements exception the last one") {
        List.init(List(2, -3, -7, 5)) should be(List(2, -3, -7))
      }
    }
  }
}
