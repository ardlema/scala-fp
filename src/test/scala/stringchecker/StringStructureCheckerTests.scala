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

object StringStructureChecker {

  val charsMap = Map('[' -> ']', '{' -> '}', '(' -> ')')

  def check(s: String) = {
    def checkStringFormat(stringAsList: List[Char], auxStack: Stack[Char]): Boolean = {
      stringAsList match {
        case Nil => auxStack.isEmpty
        case head :: tail => {
          head match {
            case c if charsMap.isDefinedAt(c) => checkStringFormat(tail, auxStack.push(head))
            case ']' => {
              val lastElement = auxStack.headOption
              if (lastElement.isDefined && lastElement.get.equals('[')) checkStringFormat(tail, auxStack.pop)
              else false
            }
            case ')' => {
              val lastElement = auxStack.headOption
              if (lastElement.isDefined && lastElement.get.equals('(')) checkStringFormat(tail, auxStack.pop)
              else false
            }
            case '}' => {
              val lastElement = auxStack.headOption
              if (lastElement.isDefined && lastElement.get.equals('{')) checkStringFormat(tail, auxStack.pop)
              else false
            }
            case _ => checkStringFormat(tail, auxStack)
          }
        }
      }
    }
    if (s.isEmpty) true
    else checkStringFormat(s.toList, empty)
  }
}