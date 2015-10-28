package fpinscala.state


trait RNG {
  def nextInt: (Int, RNG) // Should generate a random `Int`. We'll later define other functions in terms of `nextInt`.
}

object RNG {

  // NB - this was called SimpleRNG in the book text

  case class Simple(seed: Long) extends RNG {
    def nextInt: (Int, RNG) = {
      val newSeed = (seed * 0x5DEECE66DL + 0xBL) & 0xFFFFFFFFFFFFL // `&` is bitwise AND. We use the current seed to generate a new seed.
      val nextRNG = Simple(newSeed) // The next state, which is an `RNG` instance created from the new seed.
      val n = (newSeed >>> 16).toInt // `>>>` is right binary shift with zero fill. The value `n` is our new pseudo-random integer.
      (n, nextRNG) // The return value is a tuple containing both a pseudo-random integer and the next `RNG` state.
    }
  }

  type Rand[+A] = RNG => (A, RNG)

  val int: Rand[Int] = _.nextInt

  def unit[A](a: A): Rand[A] =
    rng => (a, rng)

  def map[A, B](s: Rand[A])(f: A => B): Rand[B] =
    rng => {
      val (a, rng2) = s(rng)
      (f(a), rng2)
    }

  def nonNegativeInt(rng: RNG): (Int, RNG) = {
    val (random, rng2) = rng.nextInt
    (if (random < 0) -(random + 1) else random, rng2)
  }

  def double(rng: RNG): (Double, RNG) = {
    val (random, rng2) = rng.nextInt
    (if (random == 0) 0 else Int.MaxValue / random, rng2)
  }

  def doubleViaMap(rng: RNG): (Double, RNG) = map(nonNegativeInt)(_ / (Int.MaxValue.toDouble + 1))(rng)

  def intDouble(rng: RNG): ((Int, Double), RNG) = {
    val (randomInt, rng2) = rng.nextInt
    val (randomDouble, rng3) = double(rng2)
    ((randomInt, randomDouble), rng3)
  }

  def doubleInt(rng: RNG): ((Double, Int), RNG) = {
    val ((randomInt, randomDouble), rng2) = intDouble(rng)
    ((randomDouble, randomInt), rng2)
  }

  def double3(rng: RNG): ((Double, Double, Double), RNG) = {
    val (randomDouble1, rng2) = double(rng)
    val (randomDouble2, rng3) = double(rng2)
    val (randomDouble3, rng4) = double(rng3)
    ((randomDouble1, randomDouble2, randomDouble3), rng4)
  }

  def ints(count: Int)(rng: RNG): (List[Int], RNG) = {
    def rec(count: Int, generator: RNG, randomInts: List[Int]): (List[Int], RNG) = {
      if (count == 0)
        (randomInts, generator)
      else {
        val (randomInt, newGenerator) = generator.nextInt
        rec(count - 1, newGenerator, randomInt :: randomInts)
      }
    }
    rec(count, rng, List.empty)
  }

  def intsViaFold(count: Int)(rng: RNG): (List[Int], RNG) = {
    (1 to count).foldLeft(List.empty[Int], rng) {
      case ((randomInts, generator), _) =>
        val (randomInt, newGenerator) = generator.nextInt
        (randomInt :: randomInts, newGenerator)
    }
  }

  def intsViaSequence(count: Int): Rand[List[Int]] = sequence(List.fill(count)(int))

  def map2[A, B, C](ra: Rand[A], rb: Rand[B])(f: (A, B) => C): Rand[C] =
    rng => {
      val (randomA, rng1) = ra(rng)
      val (randomB, rng2) = rb(rng1)
      (f(randomA, randomB), rng2)
    }

  def sequence[A](fs: List[Rand[A]]): Rand[List[A]] =
    fs.foldLeft(List.empty[A], _) {
      case ((as, g), f) =>
        val (a, g1) = f(g)
        (a :: as, g1)
    }

  def flatMap[A, B](f: Rand[A])(g: A => Rand[B]): Rand[B] = ???
}

case class State[S, +A](run: S => (A, S)) {
  def map[B](f: A => B): State[S, B] =
    sys.error("todo")

  def map2[B, C](sb: State[S, B])(f: (A, B) => C): State[S, C] =
    sys.error("todo")

  def flatMap[B](f: A => State[S, B]): State[S, B] =
    sys.error("todo")
}

sealed trait Input

case object Coin extends Input

case object Turn extends Input

case class Machine(locked: Boolean, candies: Int, coins: Int)

object State {
  type Rand[A] = State[RNG, A]

  def simulateMachine(inputs: List[Input]): State[Machine, (Int, Int)] = ???
}
