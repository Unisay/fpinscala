package fpinscala.laziness

import org.scalatest.{FunSpec, MustMatchers}

class StreamTest extends FunSpec with MustMatchers {

  describe("toList") {

    it("returns non-empty list") {
      Stream(1, 2, 3, 4).toList mustBe List(1, 2, 3, 4)
    }

    it("returns empty list") {
      Stream().toList mustBe Nil
    }

  }

  describe("take") {

    it("returns stream of the first n elements") {
      Stream(1, 2, 3, 4).take(0).toList mustBe List()
      Stream(1, 2, 3, 4).take(1).toList mustBe List(1)
      Stream(1, 2, 3, 4).take(2).toList mustBe List(1, 2)
      Stream(1, 2, 3, 4).take(3).toList mustBe List(1, 2, 3)
      Stream(1, 2, 3, 4).take(4).toList mustBe List(1, 2, 3, 4)
    }

    it("returns all stream if n > Stream.length") {
      Stream(1, 2).take(4).toList mustBe List(1, 2)
    }

    it("returns empty stream") {
      Stream().take(4) mustBe Empty
    }

  }

  describe("drop") {

    it("returns stream without the first n elements") {
      Stream(1, 2, 3, 4).drop(2).toList mustBe List(3, 4)
    }

    it("returns all stream if n > Stream.length") {
      Stream(1, 2) drop 4 mustBe Empty
    }

    it("returns empty stream") {
      Stream() drop 4 mustBe Empty
    }

  }

  describe("takeWhile") {

    it("returns empty stream if predicate is always false") {
      Stream(1, 3, 5, 7).takeWhile(_ < 0).toList mustBe Nil
    }

    it("returns prefix of the stream while predicate is true") {
      Stream(1, 3, 5, 7).takeWhile(_ < 6).toList mustBe List(1, 3, 5)
    }

    it("returns whole stream if predicate is always true") {
      Stream(1, 3, 5, 7).takeWhile(_ < 9).toList mustBe List(1, 3, 5, 7)
    }

  }

  describe("takeWhileViaFoldRight") {

    it("returns empty stream if predicate is always false") {
      Stream(1, 3, 5, 7).takeWhileViaFoldRight(_ < 0).toList mustBe Nil
    }

    it("returns prefix of the stream while predicate is true") {
      Stream(1, 3, 5, 7).takeWhileViaFoldRight(_ < 6).toList mustBe List(1, 3, 5)
    }

    it("returns whole stream if predicate is always true") {
      Stream(1, 3, 5, 7).takeWhileViaFoldRight(_ < 9).toList mustBe List(1, 3, 5, 7)
    }

  }

  describe("headOption") {

    it("some head") {
      Stream(1, 2, 3).headOption mustBe Some(1)
    }

    it("none head") {
      Empty.headOption mustBe None
    }

  }

  describe("map") {

    it("applies function to every element of a stream") {
      Stream(1, 2, 3).map(_.toString).toList mustBe List("1", "2", "3")
    }

  }

  describe("flatMap") {

    it("applies function to every element of a stream and flattens results") {
      val f: Int => Stream[Int] = e => if (e % 2 == 0) Stream(e, e * 10) else Empty
      Stream(1, 2, 3, 4).flatMap(f).toList mustBe List(2, 20, 4, 40)
    }

  }

  describe("filter") {

    it("removes element that doesn't satisfy a predicate") {
      Stream(1, 2, 3, 4).filter(_ % 2 == 0).toList mustBe List(2, 4)
    }

  }

  describe("append") {

    it("appends a stream to the end of the empty stream") {
      Empty.append(Stream(1, 2)).toList mustBe List(1, 2)
    }

    it("appends one stream to the end of another") {
      Stream(1, 2).append(Stream(3, 4)).toList mustBe List(1, 2, 3, 4)
    }

    it("appends empty stream to the end of another") {
      Stream(1, 2).append(Empty).toList mustBe List(1, 2)
    }

  }

}
