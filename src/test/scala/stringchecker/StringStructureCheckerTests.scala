package stringchecker

import org.scalatest._

import scala.collection.immutable.Stack
import scala.collection.immutable.Stack._

class StringStructureCheckerTests extends FunSpec with ShouldMatchers {

  describe("The StringStructureChecker") {

    it("should return true for an empty string") {
      val emptyString = ""
      StringStructureChecker.check(emptyString) should be(true)
    }

    it("should return true for a proper structure string") {
      val properString = "[{()}]"
      StringStructureChecker.check(properString) should be(true)
    }

    it("should return true for a proper structure string with characters in-between") {
      val properString = "[b{a()c}d]"
      StringStructureChecker.check(properString) should be(true)
    }

    it("should return true for a proper structure string with characters at first and at the end") {
      val properString = "ba[{()}cd]"
      StringStructureChecker.check(properString) should be(true)
    }

    it("should return false for an unbalanced string") {
      val properString = "[{()}"
      StringStructureChecker.check(properString) should be(false)
    }
  }
}