package io.github.fedcuit.fpinscala

import org.scalatest.{Matchers, FunSpec}

class Generify$Test extends FunSpec with Matchers{
  describe("Generify") {
    it("should able to check whether a Int array is sorted") {
      Generify.isSorted(Array(1, 2, 3, 5))(_ < _) should be(true)
      Generify.isSorted(Array(1, 2, 3, 5, 0))(_ < _) should be(false)
    }

    it("should able to check whether a String array is sorted") {
      Generify.isSorted("a big boy".split(" "))(_ < _) should be(true)
      Generify.isSorted("a big ass".split(" "))(_ < _) should be(false)
    }

    it("should able to check whether a Double array is sorted") {
      Generify.isSorted(Array(1d, 4d, 7d))(_ < _) should be(true)
      Generify.isSorted(Array(1d, 11d, 7d))(_ < _) should be(false)
    }

    it("should able to check whether a Person array is sorted") {
      case class Person(name: String, age: Int) extends Ordered[Person] {
        override def compare(that: Person): Int = this.age.compare(that.age)
      }

      Generify.isSorted(Array(Person("edfeng", 18), Person("RC7", 28), Person("Jay", 38)))(_ < _) should be(true)
      Generify.isSorted(Array(Person("edfeng", 18), Person("RC7", 28), Person("Jay", 8)))(_ < _) should be(false)
    }
  }
}
