package fpinscala.state

import fpinscala.state.RNG._
import org.scalatest.{FunSpec, MustMatchers}

class RNGTest extends FunSpec with MustMatchers {

  case class TestRNG(seeds: Int*) extends RNG {
    override def nextInt: (Int, RNG) = (seeds.head, TestRNG(seeds.tail: _*))
  }

  describe("nonNegativeInt") {

    it("generates non-negative integers") {
      val (random1, rng1) = nonNegativeInt(TestRNG(Int.MinValue, Int.MaxValue, -3))
      val (random2, rng2) = nonNegativeInt(rng1)
      val (random3, _) = nonNegativeInt(rng2)
      (random1, random2, random3) mustBe(Int.MaxValue, Int.MaxValue, 2)
    }

  }

  describe("double") {

    it("generates random double values between [0, 1)") {
      val (random1, rng1) = double(TestRNG(Int.MinValue, Int.MaxValue, -3))
      val (random2, rng2) = double(rng1)
      val (random3, _) = double(rng2)
      (random1, random2, random3) mustBe(0.0, 1.0, -7.15827882E8)
    }

  }

  describe("ints") {

    it("generates a list of random integers") {
      val predefinedValues = List(1, 2, 3)
      val (randomIntegers, _) = ints(3)(TestRNG(predefinedValues: _*))
      randomIntegers mustEqual predefinedValues.reverse
    }

    it("generates a list of random integers (via fold)") {
      val predefinedValues = List(1, 2, 3)
      val (randomIntegers, _) = intsViaFold(3)(TestRNG(predefinedValues: _*))
      randomIntegers mustEqual predefinedValues.reverse
    }

    it("generates a list of random integers (via sequence)") {
      val predefinedValues = List(1, 2, 3)
      val (randomIntegers, _) = intsViaSequence(3)(TestRNG(predefinedValues: _*))
      randomIntegers mustEqual predefinedValues.reverse
    }

  }

}
