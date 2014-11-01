package de.khamrakulov.algorithms

import org.scalatest._

class Chapter1Spec extends FlatSpec with Matchers {
  "A merge sort" should "return sorted list [0,1,2,3,4,5,6,7,8,9]" in {
    assertResult(List(0, 1, 2, 3, 4, 5, 6, 7, 8, 9)) {
      Chapter1.mergeSort(List(8, 3, 0, 6, 1, 7, 2, 4, 5, 9))
    }
  }
}
