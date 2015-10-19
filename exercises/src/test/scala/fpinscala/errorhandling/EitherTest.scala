package fpinscala.errorhandling

import org.scalatest.{FunSpec, MustMatchers}

class EitherTest extends FunSpec with MustMatchers {

  val left: Either[String, Int] = Left("e")
  val right: Either[String, Int] = Right(1)
  val f = (i: Int) => i * 2
  val fe = (i: Int) => Right(f(i))

  describe("map") {

    it("returns Left for Left") {
      left map f mustBe left
    }

    it("returns mapped Right[B] for Right[A]") {
      right map f mustBe Right(2)
    }

  }

  describe("flatMap") {

    it("returns Left for Left") {
      left flatMap fe mustBe left
    }

    it("returns mapped Right[B] for Right[A]") {
      right flatMap fe mustBe Right(2)
    }

  }

  describe("orElse") {

    it("returns other arg for left") {
      left orElse Right(100) mustBe Right(100)
    }

    it("returns self for right") {
      right orElse Right(100) mustBe right
    }

  }

  describe("map2") {

    it("returns Right(f(a, b)) if this is right and arg is right") {
      right.map2(Right(2))(_ + _) mustBe Right(3)
    }

    it("returns Left(self error) if this is left and arg is right") {
      left.map2(Right(2))(_ + _) mustBe left
    }

    it("returns Left(arg error) if this is right and arg is left") {
      right.map2(left)(_ + _) mustBe left
    }

    it("returns Left(self error) if this is left and arg is left") {
      left.map2(Left("arg error"))(_ + _) mustBe left
    }

  }

}
