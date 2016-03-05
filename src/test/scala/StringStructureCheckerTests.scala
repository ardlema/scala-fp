package org.ardlema.scalafp

import org.scalatest._

class StringStructureCheckerTests extends FunSpec with ShouldMatchers {

  describe("The StringStructureChecker") {

    it("should return true for a empty string") {
      val emptyString = ""
      StringStructureChecker.check(emptyString) should be(true)
    }
    
    it("should return true for a proper structure string") {
      val properString = "[({})]"
      StringStructureChecker.check(properString) should be(true)
    }
  }
}

object StringStructureChecker {

  def check(s: String) = if (s.isEmpty) true else false
}