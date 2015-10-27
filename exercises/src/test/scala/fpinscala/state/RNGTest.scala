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

}
