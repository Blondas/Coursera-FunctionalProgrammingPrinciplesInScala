package week4.lecture7_lists

import org.scalatest.{FlatSpec, Matchers}

class InsertionSortSpec extends FlatSpec with Matchers{
  val iS = new InsertionSort()

  import iS._

  "iSort" should "sort list in ascending order" in {
    val l = List(3,2,1,5)
    iSort(l) shouldEqual List(1,2,3,5)
  }

  "iSort" should "sort empty list" in {
    val l = List()
    iSort(l) shouldEqual List()
  }

  "iSort" should "sort one element list" in {
    val l = List(1)
    iSort(l) shouldEqual List(1)
  }

  "iSort" should "sort list with duplicated elements" in {
    val l = List(3,2,1,2,5)
    iSort(l) shouldEqual List(1,2,2,3,5)
  }

  "iSort" should "sort sorted list with reverse order" in {
    val l = List(5,4,3,2,1)
    iSort(l) shouldEqual List(1,2,3,4,5)
  }
}
