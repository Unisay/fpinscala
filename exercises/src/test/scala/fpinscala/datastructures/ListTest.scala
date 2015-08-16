package fpinscala.datastructures

import fpinscala.datastructures.List._
import org.scalatest.{FunSpec, MustMatchers}

class ListTest extends FunSpec with MustMatchers {

  describe("setHead") {

    it("throws exception for the empty list") {
      val error = the[java.lang.RuntimeException] thrownBy setHead(Nil, 1)
      error.getMessage mustBe "setHead on empty list"
    }

    it("replaces head of the non-empty list") {
      setHead(List(1, 2, 3), 4) mustBe List(4, 2, 3)
    }

  }


  describe("tail") {

    it("returns list without head") {
      tail(List(1, 2, 3)) mustBe List(2, 3)
    }

    it("returns empty tail for the 1-element list") {
      tail(List(1)) mustBe Nil
    }

    it("throws exception for the empty list tail ") {
      val error = the[java.lang.RuntimeException] thrownBy tail(Nil)
      error.getMessage mustBe "tail of empty list"
    }

  }

  describe("drop") {

    it("returns empty list when invoked on empty list") {
      drop(Nil, -10) mustBe Nil
      drop(Nil, 0) mustBe Nil
      drop(Nil, 10) mustBe Nil
    }

    it("returns empty list when n == list length") {
      drop(List(1, 2, 3), 3) mustBe Nil
    }

    it("returns empty list when n > list length") {
      drop(List(1, 2, 3), 4) mustBe Nil
    }

    it("returns remainder of the list when n < list length") {
      drop(List(1, 2, 3, 4), 2) mustBe List(3, 4)
    }

  }

  describe("dropWhile") {

    it("returns empty list when invoked on empty list") {
      dropWhile(Nil, (h: Any) => true) mustBe Nil
    }

    it("returns remainder of the list") {
      dropWhile(List(1, 2, 3), (e: Int) => e < 3) mustBe List(3)
    }

  }

  describe("init") {

    it("throws exception when invoked on empty list") {
      val error = the[java.lang.RuntimeException] thrownBy init(Nil)
      error.getMessage mustBe "init on empty list"
    }

    it("returns list without its last element") {
      init(List(1, 2, 3, 4)) mustBe List(1, 2, 3)
    }

  }

  describe("lengthViaFoldRight") {

    it("returns 0 for the empty list") {
      List.lengthViaFoldRight(Nil: List[Int]) mustBe 0
    }

    it("returns length of the list") {
      List.lengthViaFoldRight(List(1, 2, 3)) mustBe 3
    }

  }

  describe("foldLeft") {

    it("lengthViaFoldLeft returns 0 for the empty list") {
      lengthViaFoldLeft(Nil: List[Int]) mustBe 0
    }

    it("lengthViaFoldLeft returns length of the non-empty list") {
      lengthViaFoldLeft(List(1, 2, 3)) mustBe 3
    }

    it("sumLeft returns sum of the list") {
      sumLeft(List(1, 2, 3, 4)) mustBe 10
    }

    it("productLeft returns product of the list") {
      productLeft(List(1, 2, 3)) mustBe 6
    }

  }

  describe("reverse") {

    it("returns empty list as is") {
      reverse(Nil) mustBe Nil
    }

    it("returns non empty list reversed") {
      reverse(List(1, 2, 3)) mustBe List(3, 2, 1)
    }

  }

  describe("append") {

    it("append2 returns lists concatenated") {
      appendViaFoldRight(List(1, 2, 3), List(4, 5, 6)) mustBe List(1, 2, 3, 4, 5, 6)
    }

    it("append3 returns lists concatenated") {
      appendViaFoldLeft(List(1, 2, 3), List(4, 5, 6)) mustBe List(1, 2, 3, 4, 5, 6)
    }

  }

  it("flatten returns flattened list") {
    flatten(List(List(1, 2), List(2, 3), List(3, 4))) mustBe List(1, 2, 2, 3, 3, 4)
  }

  it("add1 adds 1 to each element of a list") {
    add1(List(1, 2, 3)) mustBe List(2, 3, 4)
  }

  it("doublesToStrings transforms list of doubles to list of strings") {
    doubles2strings(List(1, 2, 3)) mustBe List("1.0", "2.0", "3.0")
  }

  it("map applies f to each elements of a list") {
    map(List(1, 2, 3))(_ + 1) mustBe List(2, 3, 4)
  }
}
