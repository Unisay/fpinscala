package fpinscala.laziness

import fpinscala.laziness.Stream._

trait Stream[+A] {

  def toList: List[A] = this match {
    case Cons(h, t) => h() :: t().toList
    case _ => Nil
  }

  def isEmpty: Boolean = this == Empty

  def isNotEmpty: Boolean = !isEmpty

  def foldRight[B](z: => B)(f: (A, => B) => B): B = // The arrow `=>` in front of the argument type `B` means
  // that the function `f` takes its second argument by name and may choose not to evaluate it.
    this match {
      case Cons(h, t) => f(h(), t().foldRight(z)(f)) // If `f` doesn't evaluate its second argument, the recursion never occurs.
      case _ => z
    }

  def exists(p: A => Boolean): Boolean =
    foldRight(false)((a, b) => p(a) || b)

  // Here `b` is the unevaluated recursive step that folds the tail of the stream.
  // If `p(a)` returns `true`, `b` will never be evaluated and the computation terminates early.

  @annotation.tailrec
  final def find(f: A => Boolean): Option[A] = this match {
    case Empty => None
    case Cons(h, t) => if (f(h())) Some(h()) else t().find(f)
  }

  def take(n: Int): Stream[A] = this match {
    case Cons(h, t) if n > 0 => cons(h(), t().take(n - 1))
    case _ => empty
  }

  def takeViaUnfold(n: Int): Stream[A] = unfold((this, n)) {
    case (Cons(h, t), nn) if nn > 0 => Some(h(), (t(), nn - 1))
    case _ => None
  }

  def drop(n: Int): Stream[A] = this match {
    case Cons(_, t) if n > 0 => t().drop(n - 1)
    case _ => this
  }

  def takeWhile(p: A => Boolean): Stream[A] = this match {
    case Cons(h, t) if p(h()) => cons(h(), t().takeWhile(p))
    case _ => empty
  }

  def takeWhileViaUnfold(p: A => Boolean): Stream[A] = unfold(this) {
    case Cons(h, t) if p(h()) => Some(h(), t())
    case _ => None
  }

  def takeWhileViaFoldRight(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else empty)

  def forAll(p: A => Boolean): Boolean =
    foldRight(true)((a, b) => p(a) && b)

  def headOption: Option[A] =
    foldRight(Option.empty[A])((h, _) => Some(h))

  def map[B](f: A => B): Stream[B] =
    foldRight(empty[B])((h, t) => cons(f(h), t))

  def mapViaUnfold[B](f: A => B): Stream[B] = unfold(this) {
    case Cons(h, t) => Some(f(h()), t())
    case _ => None
  }

  def filter(p: A => Boolean): Stream[A] =
    foldRight(empty[A])((h, t) => if (p(h)) cons(h, t) else t)

  def append[E >: A](e: => Stream[E]): Stream[E] =
    foldRight(e)((h, t) => cons(h, t))

  def flatMap[B](f: A => Stream[B]): Stream[B] =
    foldRight(empty[B])((h, t) => f(h) append t)

  def zipWithViaUnfold[B, C](bs: Stream[B])(f: (A, B) => C) = unfold((this, bs)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some(f(ah(), bh()), (at(), bt()))
    case _ => None
  }

  def zipAll[B](bs: Stream[B]): Stream[(Option[A], Option[B])] = unfold((this, bs)) {
    case (Cons(ah, at), Cons(bh, bt)) => Some((Some(ah()), Some(bh())), (at(), bt()))
    case (Empty, Cons(bh, bt)) => Some((None, Some(bh())), (empty, bt()))
    case (Cons(ah, at), Empty) => Some((Some(ah()), None), (at(), empty))
    case _ => None
  }

  def startsWith[B](that: Stream[B]): Boolean =
    zipAll(that).takeWhile(_._2.isDefined).forAll(p => p._1 == p._2)

  def startsWithRegularRecursion[B](that: Stream[B]): Boolean = (this, that) match {
    case (_, Empty) => true
    case (Cons(h1, t1), Cons(h2, t2)) if h1() == h2() => t1().startsWithRegularRecursion(t2())
    case _ => false
  }

  def tails: Stream[Stream[A]] = unfold(this) {
    case s@Cons(_, t) => Some((s, t()))
    case Empty => None
  }.append(Stream(empty))

}

case object Empty extends Stream[Nothing]

case class Cons[+A](h: () => A, t: () => Stream[A]) extends Stream[A]

object Stream {
  def cons[A](hd: => A, tl: => Stream[A]): Stream[A] = {
    lazy val head = hd
    lazy val tail = tl
    Cons(() => head, () => tail)
  }

  def empty[A]: Stream[A] = Empty

  def apply[A](as: A*): Stream[A] =
    if (as.isEmpty) empty
    else cons(as.head, apply(as.tail: _*))

  def constant(c: Int): Stream[Int] = Stream.cons(c, constant(c))

  val ones: Stream[Int] = constant(1)

  def from(n: Int): Stream[Int] = cons(n, from(n + 1))

  val fibs: Stream[Int] = {
    def _fib(p1: Int, p2: Int): Stream[Int] = cons(p1, _fib(p2, p1 + p2))
    _fib(0, 1)
  }

  def unfold[A, S](z: S)(f: S => Option[(A, S)]): Stream[A] =
    f(z) match {
      case Some((a, s)) => cons(a, unfold(s)(f))
      case None => empty
    }

  def onesViaUnfold: Stream[Int] = unfold(1)(s => Some(s, s))

  def constantViaUnfold[T](c: T): Stream[T] = unfold(c)(s => Some(s, s))

  def fromViaUnfold(n: Int): Stream[Int] = unfold(n)(s => Some(s, s + 1))

  def fibsViaUnfold: Stream[Int] = unfold((0, 1))(p => Some(p._1, (p._2, p._1 + p._2)))

}