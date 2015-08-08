package fpinscala.gettingstarted

import fpinscala.gettingstarted.PolymorphicFunctions._
import org.scalatest.FunSuite

class PolymorphicFunctionsTest extends FunSuite {

  test("empty array is sorted") {
    assert(isSorted[Int](Array.empty, _ < _))
  }

  test("1 element array is sorted") {
    assert(isSorted[Int](Array(1), _ < _))
  }

  test("sorted array is sorted") {
    assert(isSorted[Int](Array(-1, 2, 3, 4, 50), _ < _))
  }

  test("reverse-sorted array is not sorted") {
    assert(!isSorted[Int](Array(5, 4, 2, 1), _ < _))
  }

  test("unsorted array is not sorted") {
    assert(!isSorted[Int](Array(1, 2, 3, 4, 5, 4), _ < _))
  }

  test("curry") {
    val f = (a: Int, b: Int) => a + b
    val curried = curry(f)

    assert(curried(1)(2) === 3)
  }

  test("uncurry") {
    val f = (a: Int, b: Int) => a + b
    val curried = curry(f)
    val uncurried = uncurry(curried)

    assert(uncurried(1, 2) === 3)
  }

  test("compose") {
    val f1: (String) => Array[Byte] = _.getBytes
    val f2: (Int) => String = _.toString

    val composed = compose(f1, f2)
    assert(composed(5) === Array(53))
  }

}
