package week4

import org.scalatest.{FlatSpec, Matchers}

class ListSpec extends FlatSpec with Matchers {
  "List()" should "produce an empty list" in {
    List() should have size 0
  }
  "List(1)" should "produce a list with one element: 1" in {
    List(1) should  contain only 1
  }
  "List()" should "produce a lsit with two elements: 1,2" in {
    List(1,2) should contain allOf (1,2)
  }
}
