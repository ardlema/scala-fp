package org.ardlema.scalafp

import org.scalatest._

class MutableListOpsTests extends FunSpec with ShouldMatchers {

  describe("The mutable list ops object") {
    val myList = List(1, 2, 3, 4, 5)

    it("should drop the first n elements from the list") {
      MutableListOps.drop(myList, 3) should be(List(4, 5))
    }

    it("should drop all the elements of the list when n>size of the list") {
      MutableListOps.drop(myList, 7) should be(List())
    }

    it("should drop no elements when n<=0") {
      MutableListOps.drop(myList, 0) should be(myList)
    }

    it("should drop all the elements but the last one when n=size-1") {
      MutableListOps.drop(myList, 4) should be(List(5))
    }
  }
}

