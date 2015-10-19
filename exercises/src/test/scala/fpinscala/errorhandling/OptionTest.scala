package fpinscala.errorhandling

import fpinscala.errorhandling.Option.{map2, sequence}
import org.scalatest.{FunSpec, MustMatchers}

class OptionTest extends FunSpec with MustMatchers {

  val some1: Option[Int] = Some(1)
  val none: Option[Int] = None

  describe("map") {

    it("returns result of function applied for Some") {
      some1 map (_ + 1) mustBe Some(2)
    }

    it("returns None for None") {
      none map (_ + 1) mustBe None
    }

  }

  describe("getOrElse") {

    it("returns Some") {
      some1 getOrElse "2" mustBe 1
    }

    it("returns default for None") {
      none getOrElse "2" mustBe "2"
    }

  }

  describe("orElse") {

    it("returns Some") {
      some1 orElse Some("2") mustBe some1
      some1 orElse None mustBe some1
    }

    it("returns default option for None") {
      none orElse Some(2) mustBe Some(2)
      none orElse None mustBe None
    }

  }

  describe("flatMap") {

    it("returns Some result of function applied for Some") {
      some1 flatMap (a => Some(a.toString)) mustBe Some("1")
    }

    it("returns None result of function applied for Some") {
      some1 flatMap (_ => None) mustBe None
    }

    it("returns None for None when function returns Some") {
      none flatMap (a => Some(a.toString)) mustBe None
    }

    it("returns None for None when function returns None") {
      none flatMap (_ => None) mustBe None
    }

  }

  describe("filter") {

    it("returns None for None") {
      none filter (_ > 5) mustBe None
      none filter (_ < 5) mustBe None
    }

    it("returns Some when doesn't satisfy predicate") {
      some1 filter (_ > 5) mustBe None
    }

    it("returns Some when satisfies predicate") {
      some1 filter (_ < 5) mustBe some1
    }

  }

  describe("map2") {

    it("returns Some when applied to Some and Some") {
      map2(Some(2), Some(3))(_ + _) mustBe Some(5)
    }

    it("returns None when applied to None and None") {
      map2(none, none)(_ + _) mustBe none
    }

    it("returns None when applied to None and Some") {
      map2(none, Some(3))(_ + _) mustBe none
    }

    it("returns None when applied to Some and None") {
      map2(Some(2), none)(_ + _) mustBe none
    }

  }

  describe("sequence") {

    it("returns Some(Nil) for Nil") {
      sequence(Nil) mustBe Some(Nil)
    }

    it("returns Some of list") {
      sequence(List(Some(1), Some(2), Some(3))) mustBe Some(List(1, 2, 3))
    }

    it("returns None when list contains None") {
      sequence(List(Some(1), Some(2), None)) mustBe None
    }
  }

}
