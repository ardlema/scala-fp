import org.ardlema.scalafp.InmutableList
import org.scalatest._

class InmutableListOpsTests extends FunSpec with ShouldMatchers {

  describe("The mutable list ops object") {
    val myList = InmutableList(1, 2, 3, 4, 5)

    it("should drop the first n elements from the list") {
      InmutableList.drop(myList, 3) should be(InmutableList(4, 5))
    }

    it("should drop all the elements of the list when n>size of the list") {
      InmutableList.drop(myList, 7) should be(InmutableList())
    }

    it("should drop no elements when n<=0") {
      InmutableList.drop(myList, 0) should be(myList)
    }

    it("should drop all the elements but the last one when n=size-1") {
      InmutableList.drop(myList, 4) should be(InmutableList(5))
    }
  }
}

