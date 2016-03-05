package list

import scala.collection.mutable.ListBuffer

object MutableListOps {

  def drop[A](l: List[A], n: Int): List[A] =
    if (n <= 0) l
    else {
      if (n > l.size) List()
      else {
        var mutableList = new ListBuffer[A]
        for (i<-n to l.size-1) mutableList += l(i)
        mutableList.toList
      }
    }
}

