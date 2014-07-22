package io.github.fedcuit.fpinscala.laziness

object Laziness {
  def printCaption(caption: String) {
    println()
    println(caption)
    println("*" * 20)
  }

  def main(args: Array[String]) {
    printCaption("Call by value/Strict function/Evaluate function parameter immediately")
    def callByValue(x: Int) {
      println("I'm the method body")
    }

    callByValue({
      println("evaluate parameter before invoke method body")
      20
    })

    printCaption("Call by name/Non-Strict function/Evaluate function parameter when referenced in function")

    def callByName(x: => Int) {
      println("evaluation will not happen until param is referenced in method body")
      val y = x
      println("evaluation result will not be cached, so evaluation happens every time it's referenced")
      val z = x
    }

    callByName({
      println("this line will be printed when evaluate parameter");
      20
    })

    printCaption("Lazy keyword in Scala")

    def useLazyKeyword(x: => Int) {
      println("evaluation will not happen until param is referenced in method body")
      println("lazy will delay the evaluation of parameter")
      lazy val y = x
      println("lazy will delay the evaluation of parameter")
      lazy val z = x
      println("the evaluation of parameter happens when it's not referenced lazily")
      println(x, y, z)
    }

    useLazyKeyword({
      println("this line will be printed when evaluate parameter")
      20
    })
  }

}
