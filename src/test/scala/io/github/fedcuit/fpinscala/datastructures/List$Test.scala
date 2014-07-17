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

    describe("head method") {
      it("should throw exception when access head of a empty list") {
        intercept[RuntimeException] {
          List.head(Nil)
        }
      }

      it("should return head of a list") {
        List.head(List(1, 2, 3)) should be(1)
        List.head(List(1)) should be(1)
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

    describe("append method") {
      it("should append list B to the end of list A") {
        List.append(List(1, 2, 3), List(4, 5)) should be(List(1, 2, 3, 4, 5))
        List.append2(List(1, 2, 3), List(4, 5)) should be(List(1, 2, 3, 4, 5))
      }
    }

    describe("sum method") {
      it("should return the sum of all elements") {
        List.sum(List(1, 2, 3, 4, 5)) should be(15)
        List.sum2(List(1, 2, 3, 4, 5)) should be(15)
      }
    }

    describe("product method") {
      it("should return the product of all elements") {
        List.product(List(1, 2, 3, 4, 5)) should be(120)
        List.product2(List(1, 2, 3, 4, 5)) should be(120)
      }
    }

    describe("reverse method") {
      it("should return the list in reversed order") {
        List.reverse(List(1, 2, 3, 4, 5)) should be(List(5, 4, 3, 2, 1))
      }
    }

    describe("length method") {
      it("should return the size of elements in list") {
        List.length(List(1, 2, 3, 4)) should be(4)
        List.length2(List(1, 2, 3, 4)) should be(4)
      }

      it("should return 0 when call in on a empty list") {}
      List.length(List()) should be(0)
    }

    describe("last method") {
      it("should throw exception when call on empty list") {
        intercept[RuntimeException] {
          List.last(Nil)
        }
      }

      it("should return the last element") {
        List.last(List(1, 2, 3, 4, 5)) should be(5)
      }
    }

    describe("concat method") {
      it("should join several list into a single list") {
        List.concat(List(1, 2, 3), List(4, 5, 6), List(7, 8, 9)) should be(List(1, 2, 3, 4, 5, 6, 7, 8, 9))
      }
    }

    describe("map method") {
      it("should return a new list in which each element is calculated base on the old one") {
        List.map(List(1, 2, 3, 4))(_ + 1) should be(List(2, 3, 4, 5))
        List.map(List(1d, 2d, 3d, 4d))(_.toString) should be(List("1.0", "2.0", "3.0", "4.0"))
      }
    }

    describe("foldLeft method") {
      it("should fold from left") {
        List.foldLeft(List(1, 2, 3, 4), List[Int]())((ys, x) => Cons(x, ys)) should be(List(4, 3, 2, 1))
      }
    }

    describe("foldRight method") {
      it("should fold from right") {
        List.foldRight(List(1, 2, 3, 4), List[Int]())((ys, x) => Cons(x, ys)) should be(List(1, 2, 3, 4))
        List.foldRight2(List(1, 2, 3, 4), List[Int]())((ys, x) => Cons(x, ys)) should be(List(1, 2, 3, 4))
      }
    }

    describe("filter method") {
      it("should remove all elements which meets condition") {
        List.filter(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
        List.filter2(List(1, 2, 3, 4))(_ % 2 == 0) should be(List(2, 4))
      }
    }

    describe("flatMap method") {
      it("should map each element into a list and flatten the lists into a single list") {
        List.flatMap(List(1, 2, 3, 4))(x => List(x, x)) should be(List(1, 1, 2, 2, 3, 3, 4, 4))
      }
    }

    describe("plus method") {
      it("should zip two lists as one in which each element is sum of elements on corresponding position") {
        List.zipTo(List(1, 2, 3), List(4, 5, 6))(_ + _) should be(List(5, 7, 9))
        List.zipTo(List(1, 2, 3), List("this", "is", "me"))(_ + _.length) should be(List(5, 4, 5))
      }
    }

    describe("take method") {
      it("should return the first n elements") {
        List.take(List(1, 2, 3, 4, 5, 6), 3) should be(List(1, 2, 3))
      }

      it("should return whole list if n is bigger than the length of the list") {
        List.take(List(1, 2, 3, 4, 5, 6), 8) should be(List(1, 2, 3, 4, 5, 6))
      }
    }

    describe("takeWhile method") {
      it("should the longest valid prefix") {
        List.takeWhile(List(1, 2, 3, 4, 5, 6))(_ < 4) should be(List(1, 2, 3))
      }
    }

    describe("forAll method") {
      it("should return true only if all elements meets condition") {
        List.forAll(List(2, 4, 6, 8, 10))(_ % 2 == 0) should be(true)
        List.forAll(List(2, 4, 5, 6, 7))(_ % 2 == 0) should be(false)
      }
    }

    describe("exists method") {
      it("should return true if any element meets condition") {
        List.exists(List(1, 3, 5, 6, 7))(_ % 2 == 0) should be(true)
        List.exists(List(1, 3, 5, 9, 7))(_ % 2 == 0) should be(false)
      }
    }

    describe("hasSubSequence method") {
      it("should return true when list A contains list B") {
        List.hasSubSequence(List(1, 2, 3, 4), List(2, 3)) should be(true)
        List.hasSubSequence(List(1, 2, 3, 4), List(1, 2, 3)) should be(true)
        List.hasSubSequence(List(1, 2, 3, 4), List(2, 3, 4)) should be(true)
      }
    }
  }
}
