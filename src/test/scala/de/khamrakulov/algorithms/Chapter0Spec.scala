package de.khamrakulov.algorithms

import org.scalatest._

class Chapter0Spec extends FlatSpec with Matchers {

  "A Fibonacci sequence" should "return zero if started with 0" in {
    assertResult(0) {
      Chapter0.fib(0)
    }
  }

  it should "return 1 for 1" in {
    assertResult(1)
    {
      Chapter0.fib(1)
    }
  }

  it should "return 1 for 2" in {
    assertResult(1)
    {
      Chapter0.fib(2)
    }
  }

  it should "return 55 for 10" in {
    assertResult(55)
    {
      Chapter0.fib(10)
    }
  }
}
