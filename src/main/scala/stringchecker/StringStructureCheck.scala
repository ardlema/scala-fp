package stringchecker

import scala.annotation.tailrec
import scala.collection.immutable.Stack
import scala.collection.immutable.Stack._

object StringStructureChecker {

  val charsMap = Map('[' -> ']', '{' -> '}', '(' -> ')')
  val charsMapSwapped = charsMap map {_.swap}

  def check(s: String) = {
    @tailrec
    def checkStringFormat(stringAsList: List[Char], auxStack: Stack[Char]): Boolean = {
      stringAsList match {
        case Nil => auxStack.isEmpty
        case head :: tail => {
          head match {
            case openStructureChar if charsMap.isDefinedAt(openStructureChar) =>
              checkStringFormat(tail, auxStack.push(head))
            case closeStructureChar if charsMapSwapped.isDefinedAt(closeStructureChar) => {
              val lastElement = auxStack.headOption
              if (lastElement.isDefined &&
                lastElement.get.equals(charsMapSwapped.get(closeStructureChar).get)) checkStringFormat(tail, auxStack.pop)
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
