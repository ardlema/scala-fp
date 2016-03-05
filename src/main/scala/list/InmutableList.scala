package list

sealed trait InmutableList[+A] // `List` data type, parameterized on a type, `A`
case object Nil extends InmutableList[Nothing] // A `List` data constructor representing the empty list
case class Cons[+A](head: A, tail: InmutableList[A]) extends InmutableList[A]

object InmutableList {

  def apply[A](as: A*): InmutableList[A] = // Variadic function syntax
    if (as.isEmpty) Nil
    else Cons(as.head, apply(as.tail: _*))

  @annotation.tailrec
  def drop[A](l: InmutableList[A], n: Int): InmutableList[A] =
    if (n <= 0) l
    else l match {
      case Nil => Nil
      case Cons(_, t) => drop(t, n-1)
    }
}

