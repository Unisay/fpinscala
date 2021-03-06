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

  describe("takeViaUnfold") {

    it("returns stream of the first n elements") {
      Stream(1, 2, 3, 4).takeViaUnfold(0).toList mustBe List()
      Stream(1, 2, 3, 4).takeViaUnfold(1).toList mustBe List(1)
      Stream(1, 2, 3, 4).takeViaUnfold(2).toList mustBe List(1, 2)
      Stream(1, 2, 3, 4).takeViaUnfold(3).toList mustBe List(1, 2, 3)
      Stream(1, 2, 3, 4).takeViaUnfold(4).toList mustBe List(1, 2, 3, 4)
    }

    it("returns all stream if n > Stream.length") {
      Stream(1, 2).takeViaUnfold(4).toList mustBe List(1, 2)
    }

    it("returns empty stream") {
      Stream().takeViaUnfold(4) mustBe Empty
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

  describe("takeWhileViaUnfold") {

    it("returns empty stream if predicate is always false") {
      Stream(1, 3, 5, 7).takeWhileViaUnfold(_ < 0).toList mustBe Nil
    }

    it("returns prefix of the stream while predicate is true") {
      Stream(1, 3, 5, 7).takeWhileViaUnfold(_ < 6).toList mustBe List(1, 3, 5)
    }

    it("returns whole stream if predicate is always true") {
      Stream(1, 3, 5, 7).takeWhileViaUnfold(_ < 9).toList mustBe List(1, 3, 5, 7)
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

  describe("mapViaUnfold") {

    it("applies function to every element of a stream") {
      Stream(1, 2, 3).mapViaUnfold(_.toString).toList mustBe List("1", "2", "3")
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

  describe("constant") {

    it("returns infinite stream of constants") {
      Stream.constant(5).map(_ - 4).take(5).toList mustBe List(1, 1, 1, 1, 1)
    }

  }

  describe("from") {

    it("returns growing row of integers") {
      Stream.from(3).take(5).toList mustBe List(3, 4, 5, 6, 7)
    }

  }

  describe("fibs") {

    it("returns fibonacci sequence") {
      Stream.fibs.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }

  }

  describe("constantViaUnfold") {

    it("returns infinite stream of constants") {
      Stream.constantViaUnfold(5).map(_ - 4).take(5).toList mustBe List(1, 1, 1, 1, 1)
    }

  }

  describe("fromViaUnfold") {

    it("returns growing row of integers") {
      Stream.fromViaUnfold(3).take(5).toList mustBe List(3, 4, 5, 6, 7)
    }

  }

  describe("fibsViaUnfold") {

    it("returns fibonacci sequence") {
      Stream.fibsViaUnfold.take(7).toList mustBe List(0, 1, 1, 2, 3, 5, 8)
    }

  }

  describe("zipWithViaUnfold") {

    it("zips streams of equal length") {
      Stream(1, 2, 3).zipWithViaUnfold(Stream("a", "b", "c"))(_ + "-" + _).toList mustBe List("1-a", "2-b", "3-c")
    }

    it("zips with a longer stream") {
      Stream(1, 2).zipWithViaUnfold(Stream("a", "b", "c"))(_ + "-" + _).toList mustBe List("1-a", "2-b")
    }

    it("zips with a shorter stream") {
      Stream(1, 2, 3).zipWithViaUnfold(Stream("a", "b"))(_ + "-" + _).toList mustBe List("1-a", "2-b")
    }

  }

  describe("zipAll") {

    it("zips streams of equal length") {
      Stream(1, 2).zipAll(Stream("a", "b")).toList mustBe List((Some(1), Some("a")), (Some(2), Some("b")))
    }

    it("zips with a longer stream") {
      Stream(1).zipAll(Stream("a", "b")).toList mustBe List((Some(1), Some("a")), (None, Some("b")))
    }

    it("zips with a shorter stream") {
      Stream(1, 2).zipAll(Stream("a")).toList mustBe List((Some(1), Some("a")), (Some(2), None))
    }

  }

  describe("startsWith") {

    it("returns true if stream starts with another non-empty stream") {
      Stream(1, 2, 3) startsWith Stream(1, 2) mustBe true
    }

    it("returns true if stream starts with another empty stream") {
      Stream(1, 2, 3) startsWith Empty mustBe true
    }

    it("returns true if empty stream starts with another empty stream") {
      Empty startsWith Empty mustBe true
    }

    it("returns false for an empty stream starts with another non-empty stream") {
      Empty startsWith Stream(1) mustBe false
    }

    it("returns false if stream starts with another non-empty stream") {
      Stream(1, 3, 4) startsWith Stream(1, 2) mustBe false
    }

  }

  describe("tails") {

    it("returns stream of tails") {
      Stream(1, 2, 3, 4).tails.map(_.toList).toList mustBe List(
        List(1, 2, 3, 4),
        List(2, 3, 4),
        List(3, 4),
        List(4),
        List())
    }

    it("returns empty stream for empty stream") {
      Empty.tails.toList mustBe List(Empty)
    }

  }

  describe("scanRight") {

    it("returns list of intermediate results") {
      Stream(1, 2, 3).scanRight(0)(_ + _).toList mustBe List(6, 5, 3, 0)
    }

  }

}
