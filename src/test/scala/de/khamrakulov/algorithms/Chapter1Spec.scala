package de.khamrakulov.algorithms

import org.scalatest._

class Chapter1Spec extends FlatSpec with Matchers {
  "A merge sort" should "return sorted list [0,1,2,3,4,5,6,7,8,9]" in {
    assertResult(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) {
      Chapter1.mergeSort(List(8, 3, 0, 6, 1, 7, 2, 4, 5, 9))
    }
  }

  "A Francais multiply" should "return 2468642 as result of multiplication" in {
    assertResult(2468642) {
      Chapter1.multiplyKhwarizmi(1111, 2222)
    }
  }

  "A Karatsuba multiply" should "return 2468642 as result of multiplication" in {
    assertResult("2468642") {
      Chapter1.multiplyKaratsuba("1111", "2222")
    }
  }
}
