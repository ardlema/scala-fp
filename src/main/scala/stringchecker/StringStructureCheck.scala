package stringchecker

import scala.collection.immutable.Stack
import scala.collection.immutable.Stack._

object StringStructureChecker {

  val charsMap = Map('[' -> ']', '{' -> '}', '(' -> ')')
  val charsMapSwapped = charsMap map {_.swap}

  def check(s: String) = {
    def checkStringFormat(stringAsList: List[Char], auxStack: Stack[Char]): Boolean = {
      stringAsList match {
        case Nil => auxStack.isEmpty
        case head :: tail => {
          head match {
            case c if charsMap.isDefinedAt(c) => checkStringFormat(tail, auxStack.push(head))
            case c1 if charsMapSwapped.isDefinedAt(c1) => {
              val lastElement = auxStack.headOption
              if (lastElement.isDefined &&
                lastElement.get.equals(charsMapSwapped.get(c1).get)) checkStringFormat(tail, auxStack.pop)
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
